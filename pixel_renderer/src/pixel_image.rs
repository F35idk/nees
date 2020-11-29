use std::ffi::c_void;
use std::mem;
use std::ptr;

use ash::prelude::VkResult;
use ash::version::DeviceV1_0;
use ash::{self, vk};

use super::buffer::BufferWrapper;
use super::error::VulkanError;
use super::util;

pub struct PixelImage {
    pub image: vk::Image,
    pub image_memory: vk::DeviceMemory,
    pub view: vk::ImageView,
    pub buffer: BufferWrapper,
    pub pixels_raw: *mut u8,
    pub width: u64,
    pub height: u64,
}

impl PixelImage {
    #[allow(unused_unsafe)]
    pub unsafe fn new(
        instance: &ash::Instance,
        logical_device: &ash::Device,
        physical_device: vk::PhysicalDevice,
        // NOTE: graphics_queue could instead be a separate transfer queue
        width: u64,
        height: u64,
        format: vk::Format,
        // NOTE: pixel_size must match the image format!
        pixel_size: u64,
    ) -> Result<Self, VulkanError> {
        let img_size = width * height * pixel_size;

        let buffer = unsafe {
            BufferWrapper::new_empty(
                instance,
                logical_device,
                physical_device,
                img_size,
                vk::BufferUsageFlags::TRANSFER_SRC,
                vk::SharingMode::EXCLUSIVE,
                // require buffer to be host visible
                vk::MemoryPropertyFlags::HOST_VISIBLE,
                // prefer buffer not to be host coherent
                Some(vk::MemoryPropertyFlags::HOST_COHERENT),
            )?
        };

        // map buffer memory into host memory
        let mapped = unsafe {
            match logical_device.map_memory(
                buffer.memory,
                0,
                vk::WHOLE_SIZE,
                vk::MemoryMapFlags::empty(),
            ) {
                Ok(m) => m,
                Err(e) => {
                    buffer.destroy(logical_device);
                    return Err(VulkanError::Result(e));
                }
            }
        };

        let pixels_raw: *mut u8 = unsafe { mem::transmute::<*mut c_void, _>(mapped) };

        // memset mapped buffer memory to 0
        unsafe { ptr::write_bytes::<u8>(pixels_raw, 0, img_size as usize) };

        // NOTE: we don't need to flush our writes to the mapped memory here as
        // this is taken care of before submitting commands in our draw loop

        let (image, image_memory) = unsafe {
            match util::create_empty_image(
                instance,
                logical_device,
                physical_device,
                (width as u32, height as u32),
                format,
                vk::ImageTiling::OPTIMAL,
                vk::ImageLayout::UNDEFINED,
                vk::SharingMode::EXCLUSIVE,
                vk::ImageUsageFlags::TRANSFER_DST | vk::ImageUsageFlags::SAMPLED,
                vk::MemoryPropertyFlags::DEVICE_LOCAL,
                None,
            ) {
                Ok(i) => i,
                Err(e) => {
                    logical_device.unmap_memory(buffer.memory);
                    buffer.destroy(logical_device);
                    return Err(e);
                }
            }
        };

        let view = unsafe {
            match util::create_image_view(image, format, logical_device) {
                Ok(v) => v,
                Err(e) => {
                    logical_device.destroy_image(image, None);
                    logical_device.free_memory(image_memory, None);

                    logical_device.unmap_memory(buffer.memory);
                    buffer.destroy(logical_device);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        Ok(Self {
            image,
            image_memory,
            view,
            buffer,
            pixels_raw,
            width,
            height,
        })
    }

    // called to ensure that host writes to the pixel buffer mapped memory are made
    // visible to the device
    // NOTE: since we guarantee that the device won't be writing to the pixel buffer,
    // we don't need an 'invalidate_mapped_buffer' counterpart to this function
    pub unsafe fn flush_mapped_buffer(&self, logical_device: &ash::Device) -> VkResult<()> {
        // if memory type isn't coherent we must flush our writes manually
        if self
            .buffer
            .memory_type
            .property_flags
            .contains(vk::MemoryPropertyFlags::HOST_COHERENT)
            == false
        {
            let mapped_range = [vk::MappedMemoryRange::builder()
                .memory(self.buffer.memory)
                .offset(0)
                .size(vk::WHOLE_SIZE)
                .build()];

            logical_device.flush_mapped_memory_ranges(&mapped_range)?;
        }

        Ok(())
    }

    pub unsafe fn destroy(&self, logical_device: &ash::Device) {
        logical_device.destroy_image_view(self.view, None);

        // NOTE: technically not needed, as mapped memory is implictily unmapped upon destruction
        logical_device.unmap_memory(self.buffer.memory);
        self.buffer.destroy(logical_device);

        logical_device.destroy_image(self.image, None);
        logical_device.free_memory(self.image_memory, None);
    }
}
