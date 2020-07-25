use std::mem::size_of;

use ash::prelude::VkResult;
use ash::version::{DeviceV1_0, InstanceV1_0};
use ash::{self, vk};

use super::error::VulkanError;
use super::util;

pub struct BufferWrapper {
    pub buffer: vk::Buffer,
    pub memory: vk::DeviceMemory,
    pub memory_type: vk::MemoryType,
}

impl BufferWrapper {
    // OPTIMIZE: use a dedicated memory allocator for buffer allocations
    #[allow(unused_unsafe)]
    pub unsafe fn new_empty(
        instance: &ash::Instance,
        logical_device: &ash::Device,
        physical_device: vk::PhysicalDevice,
        size: vk::DeviceSize,
        usage: vk::BufferUsageFlags,
        sharing_mode: vk::SharingMode,
        required_memory_properties: vk::MemoryPropertyFlags,
        unpreferred_memory_properties: Option<vk::MemoryPropertyFlags>,
    ) -> Result<Self, VulkanError> {
        let buffer_info = vk::BufferCreateInfo::builder()
            .size(size)
            .usage(usage)
            .sharing_mode(sharing_mode);

        let buffer = unsafe { logical_device.create_buffer(&buffer_info, None)? };
        let mem_requirements = unsafe { logical_device.get_buffer_memory_requirements(buffer) };
        let device_mem_props =
            unsafe { instance.get_physical_device_memory_properties(physical_device) };

        let mem_type_index = unsafe {
            match util::get_mem_type_index(
                required_memory_properties,
                unpreferred_memory_properties,
                mem_requirements,
                device_mem_props,
            ) {
                Ok(i) => i,
                Err(e) => {
                    logical_device.destroy_buffer(buffer, None);
                    return Err(e);
                }
            }
        };

        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(mem_requirements.size)
            .memory_type_index(mem_type_index as u32);

        let memory = unsafe {
            match logical_device.allocate_memory(&alloc_info, None) {
                Ok(i) => i,
                Err(e) => {
                    logical_device.destroy_buffer(buffer, None);
                    return Err(VulkanError::Result(e));
                }
            }
        };

        // bind memory to buffer
        unsafe {
            match logical_device.bind_buffer_memory(buffer, memory, 0) {
                Ok(i) => i,
                Err(e) => {
                    logical_device.destroy_buffer(buffer, None);
                    logical_device.free_memory(memory, None);
                    return Err(VulkanError::Result(e));
                }
            }
        }

        Ok(Self {
            buffer,
            memory,
            memory_type: device_mem_props.memory_types[mem_type_index],
        })
    }

    // creates and fills a buffer with the given data (T) in device-local memory
    #[allow(unused_unsafe)]
    pub unsafe fn new_device_local<T>(
        data: &[T],
        buffer_usage: vk::BufferUsageFlags,
        instance: &ash::Instance,
        logical_device: &ash::Device,
        physical_device: vk::PhysicalDevice,
        cmd_pool: vk::CommandPool,
        // NOTE: graphics_queue could instead be a separate transfer queue
        graphics_queue: vk::Queue,
    ) -> Result<Self, VulkanError> {
        let buf_size = (size_of::<T>() * data.len()) as u64;

        // create staging buffer
        let staging_buf = unsafe {
            Self::new_empty(
                instance,
                logical_device,
                physical_device,
                buf_size,
                vk::BufferUsageFlags::TRANSFER_SRC,
                vk::SharingMode::EXCLUSIVE,
                vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
                None,
            )?
        };

        // map staging buffer to cpu and copy the data to it
        unsafe {
            let mapped = logical_device.map_memory(
                staging_buf.memory,
                0,
                vk::WHOLE_SIZE,
                vk::MemoryMapFlags::empty(),
            )?;

            util::copy_to_mapped::<T>(&data, mapped);

            logical_device.unmap_memory(staging_buf.memory);
        }

        // OPTIMIZE: on some systems, the gpu may have memory that is host-visible,
        // host-coherent /and/ device-local. in that case, we could avoid using a
        // separate staging buffer and simply write to the buffer directly

        // create destination buffer
        let dst_buf = unsafe {
            match Self::new_empty(
                instance,
                logical_device,
                physical_device,
                buf_size,
                // destination buffer should support being transferred to in
                // addition to supporting the usage(s) passed in 'buffer_usage'
                buffer_usage | vk::BufferUsageFlags::TRANSFER_DST,
                vk::SharingMode::EXCLUSIVE,
                vk::MemoryPropertyFlags::DEVICE_LOCAL,
                None,
            ) {
                Ok(d) => d,
                Err(e) => {
                    staging_buf.destroy(logical_device);
                    return Err(e);
                }
            }
        };

        // copy from staging buffer into destination buffer
        unsafe {
            match copy_buffer(
                staging_buf.buffer,
                dst_buf.buffer,
                buf_size,
                logical_device,
                cmd_pool,
                graphics_queue,
            ) {
                Ok(d) => d,
                Err(e) => {
                    staging_buf.destroy(logical_device);
                    dst_buf.destroy(logical_device);
                    return Err(VulkanError::Result(e));
                }
            }
        };

        // clean up staging buffer
        unsafe {
            staging_buf.destroy(logical_device);
        }

        Ok(dst_buf)
    }

    pub unsafe fn destroy(&self, logical_device: &ash::Device) {
        logical_device.destroy_buffer(self.buffer, None);
        logical_device.free_memory(self.memory, None);
    }
}

#[allow(unused_unsafe)]
pub unsafe fn copy_buffer(
    src: vk::Buffer,
    dst: vk::Buffer,
    size: vk::DeviceSize,
    logical_device: &ash::Device,
    // NOTE: could be a 'transient' cmd pool
    cmd_pool: vk::CommandPool,
    // NOTE: could instead be a separate transfer queue
    graphics_queue: vk::Queue,
) -> VkResult<()> {
    let cmd_alloc_info = vk::CommandBufferAllocateInfo::builder()
        .level(vk::CommandBufferLevel::PRIMARY)
        .command_pool(cmd_pool)
        .command_buffer_count(1);

    let cmd_bufs = unsafe { logical_device.allocate_command_buffers(&cmd_alloc_info)? };

    let cmd_begin_info =
        vk::CommandBufferBeginInfo::builder().flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);

    // start recording to command buffer
    unsafe {
        match logical_device.begin_command_buffer(cmd_bufs[0], &cmd_begin_info) {
            Ok(()) => (),
            Err(e) => {
                logical_device.free_command_buffers(cmd_pool, &cmd_bufs);
                return Err(e);
            }
        }
    }

    let copy_region = [vk::BufferCopy::builder()
        // NOTE: parameterize offsets?
        .src_offset(0)
        .dst_offset(0)
        .size(size)
        .build()];

    // record copy operation
    unsafe { logical_device.cmd_copy_buffer(cmd_bufs[0], src, dst, &copy_region) };

    // stop recording
    unsafe {
        match logical_device.end_command_buffer(cmd_bufs[0]) {
            Ok(()) => (),
            Err(e) => {
                logical_device.free_command_buffers(cmd_pool, &cmd_bufs);
                return Err(e);
            }
        }
    }

    // OPTIMIZE: on some devices, there may be a separate, dedicated
    // transfer queue we can take advantage of for transfer operations

    // submit command buffer and wait for the buffer copy to complete
    let submit_info = [vk::SubmitInfo::builder().command_buffers(&cmd_bufs).build()];
    unsafe {
        let res1 = logical_device.queue_submit(graphics_queue, &submit_info, vk::Fence::null());
        let res2 = logical_device.queue_wait_idle(graphics_queue);

        match (res1, res2) {
            (Err(e), _) | (Ok(_), Err(e)) => {
                logical_device.free_command_buffers(cmd_pool, &cmd_bufs);
                return Err(e);
            }
            (Ok(_), Ok(_)) => (),
        }
    }

    // clean up command buffer
    unsafe { logical_device.free_command_buffers(cmd_pool, &cmd_bufs) };

    Ok(())
}
