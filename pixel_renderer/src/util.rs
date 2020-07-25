use core::ffi::c_void;
use std::mem::{size_of, transmute, MaybeUninit};
use std::ptr;
use std::slice;

use ash::prelude::VkResult;
use ash::version::{DeviceV1_0, InstanceV1_0};
use ash::{self, vk};

use super::error::VulkanError;

#[allow(unused_unsafe)]
pub unsafe fn create_empty_image(
    instance: &ash::Instance,
    logical_device: &ash::Device,
    physical_device: vk::PhysicalDevice,
    dimensions: (u32, u32),
    format: vk::Format,
    tiling: vk::ImageTiling,
    initial_layout: vk::ImageLayout,
    sharing_mode: vk::SharingMode,
    usage: vk::ImageUsageFlags,
    required_memory_properties: vk::MemoryPropertyFlags,
    unpreferred_memory_properties: Option<vk::MemoryPropertyFlags>,
) -> Result<(vk::Image, vk::DeviceMemory), VulkanError> {
    let image_info = vk::ImageCreateInfo::builder()
        .image_type(vk::ImageType::TYPE_2D)
        .format(format)
        .tiling(tiling)
        .initial_layout(initial_layout)
        .sharing_mode(sharing_mode)
        .mip_levels(1)
        .array_layers(1)
        .usage(usage)
        .samples(vk::SampleCountFlags::TYPE_1)
        .extent(vk::Extent3D {
            width: dimensions.0,
            height: dimensions.1,
            depth: 1,
        });

    let image = unsafe { logical_device.create_image(&image_info, None)? };

    let img_mem_requirements = unsafe { logical_device.get_image_memory_requirements(image) };
    let device_mem_props =
        unsafe { instance.get_physical_device_memory_properties(physical_device) };

    let mem_type_index = unsafe {
        match get_mem_type_index(
            required_memory_properties,
            unpreferred_memory_properties,
            img_mem_requirements,
            device_mem_props,
        ) {
            Ok(i) => i,
            Err(e) => {
                logical_device.destroy_image(image, None);
                return Err(e);
            }
        }
    };

    let alloc_info = vk::MemoryAllocateInfo::builder()
        .allocation_size(img_mem_requirements.size)
        .memory_type_index(mem_type_index as u32);

    let memory = unsafe {
        match logical_device.allocate_memory(&alloc_info, None) {
            Ok(m) => m,
            Err(e) => {
                logical_device.destroy_image(image, None);
                return Err(VulkanError::Result(e));
            }
        }
    };

    // bind memory to buffer
    unsafe {
        match logical_device.bind_image_memory(image, memory, 0) {
            Ok(()) => (),
            Err(e) => {
                logical_device.destroy_image(image, None);
                logical_device.free_memory(memory, None);
                return Err(VulkanError::Result(e));
            }
        }
    }

    Ok((image, memory))
}

// attempts to find the memory type index for the physical device that fits
// the given parameters. if there are no memory types with 'required_mem_props'
// that don't also contain 'unpreferred_mem_props', a fallback "best" candidate
// may be returned. if no fallback is found, the function returns an error
pub fn get_mem_type_index(
    required_mem_props: vk::MemoryPropertyFlags,
    unpreferred_mem_props: Option<vk::MemoryPropertyFlags>,
    buffer_mem_requirements: vk::MemoryRequirements,
    phys_device_mem_props: vk::PhysicalDeviceMemoryProperties,
) -> Result<usize, VulkanError> {
    let mut best_candidate = None;

    for (i, mem_type) in phys_device_mem_props.memory_types.iter().enumerate() {
        // if memory type corresponding to index i is supported for the buffer/image
        if buffer_mem_requirements.memory_type_bits & (1 << i) != 0 {
            // if memory type has the required properties
            if mem_type.property_flags.contains(required_mem_props) {
                if let Some(unpreferred) = unpreferred_mem_props {
                    // if memory type also has unpreferred props
                    if mem_type.property_flags.contains(unpreferred) {
                        best_candidate = Some(i);
                        continue;
                    }
                }

                return Ok(i);
            }
        }
    }

    best_candidate.ok_or(VulkanError::NoMemoryType(
        "failed to find suitable memory type for buffer/image",
    ))
}

// copies a slice of T to the memory pointed to by 'mapped'. the size of
// the memory must not be less than the size of the entire slice ('data') for
// this to be safe. used for copying into the mapped memory given by vkMapMemory()
pub unsafe fn copy_to_mapped<T>(data: &[T], mapped: *mut c_void) {
    // SAFETY: TODO: this is all very spooky and unsafe, and still
    // probably invokes U.B somehow. so take a look at this again

    // cast from void ptr and then to slice
    let data_size = (size_of::<T>() * data.len()) as u64;
    let mapped_cast: *mut MaybeUninit<T> = transmute::<*mut c_void, _>(mapped);
    let mapped_slice: &mut [MaybeUninit<T>] =
        slice::from_raw_parts_mut(mapped_cast, data_size as usize);

    // alignment must be correct
    assert!(mapped as usize % std::mem::align_of::<T>() == 0);

    // TODO: if the alignment of the mapped memory isn't correct
    // for the type, we can use ash::util::Align

    // equivalent to memcpy
    ptr::copy_nonoverlapping::<T>(&data[0], mapped_slice[0].as_mut_ptr(), data.len());
}

#[allow(unused_unsafe)]
pub unsafe fn create_image_view(
    image: vk::Image,
    format: vk::Format,
    logical_device: &ash::Device,
) -> VkResult<vk::ImageView> {
    let components = vk::ComponentMapping::builder()
        .r(vk::ComponentSwizzle::IDENTITY)
        .g(vk::ComponentSwizzle::IDENTITY)
        .b(vk::ComponentSwizzle::IDENTITY)
        .a(vk::ComponentSwizzle::IDENTITY);

    let subresource_range = vk::ImageSubresourceRange::builder()
        .aspect_mask(vk::ImageAspectFlags::COLOR)
        .base_mip_level(0)
        .level_count(1)
        .base_array_layer(0)
        .layer_count(1);

    let img_info = vk::ImageViewCreateInfo::builder()
        .image(image)
        .view_type(vk::ImageViewType::TYPE_2D)
        .format(format)
        .components(*components)
        .subresource_range(*subresource_range);

    unsafe { logical_device.create_image_view(&img_info, None) }
}

pub fn to_raw_ptrs(slice_array: &[&[i8]]) -> Vec<*const i8> {
    slice_array
        .iter()
        .map(|l| l.as_ptr())
        .collect::<Vec<*const i8>>()
}

pub fn strcmp(a: &[i8], b: &[i8]) -> bool {
    let mut a_iter = a.iter();
    let mut b_iter = b.iter();

    loop {
        match (a_iter.next(), b_iter.next()) {
            (Some(a_byte), Some(b_byte)) => {
                // reached nul on both strings
                if *a_byte == 0 && *b_byte == 0 {
                    return true;
                }

                if *a_byte == *b_byte {
                    continue;
                } else {
                    return false;
                }
            }
            // end of both strings
            (None, None) => return true,
            // one ends before the other
            (Some(a_byte), None) => {
                if *a_byte == 0 {
                    return true;
                } else {
                    return false;
                }
            }
            // one ends before the other
            (None, Some(b_byte)) => {
                if *b_byte == 0 {
                    return true;
                } else {
                    return false;
                }
            }
        }
    }
}

#[test]
fn strcmp_test() {
    let bytes1 = b"test!\0\0";
    let bytes2 = b"test!";

    let string1: &[i8] =
        unsafe { slice::from_raw_parts(bytes1.as_ptr() as *const i8, bytes1.len()) };
    let string2: &[i8] =
        unsafe { slice::from_raw_parts(bytes2.as_ptr() as *const i8, bytes2.len()) };

    assert!(strcmp(string1, string2));

    let bytes3 = b"test!\0";
    let bytes4 = b"test!\0";

    let string3: &[i8] =
        unsafe { slice::from_raw_parts(bytes3.as_ptr() as *const i8, bytes3.len()) };
    let string4: &[i8] =
        unsafe { slice::from_raw_parts(bytes4.as_ptr() as *const i8, bytes4.len()) };

    assert!(strcmp(string3, string4));

    let bytes5 = b"test!";
    let bytes6 = b"test!";

    let string5: &[i8] =
        unsafe { slice::from_raw_parts(bytes5.as_ptr() as *const i8, bytes5.len()) };
    let string6: &[i8] =
        unsafe { slice::from_raw_parts(bytes6.as_ptr() as *const i8, bytes6.len()) };

    assert!(strcmp(string5, string6));
}
