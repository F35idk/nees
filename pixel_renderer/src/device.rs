use ash::extensions::khr;
use ash::prelude::VkResult;
use ash::version::{DeviceV1_0, InstanceV1_0};
use ash::{self, vk};

use super::debug;
use super::util;

fn device_extension_names() -> [&'static [i8]; 1] {
    [unsafe { &*(khr::Swapchain::name().to_bytes_with_nul() as *const [u8] as *const [i8]) }]
}

pub struct DeviceWrapper {
    pub logical: ash::Device,
    pub physical: vk::PhysicalDevice,
    pub queues: Queues,
}

pub struct Queues {
    pub graphics: vk::Queue,
    pub graphics_family_index: u32,
    pub presentation: vk::Queue,
    pub present_family_index: u32,
}

// private helper struct to avoid having
// to pass around tuples everywhere
struct QueueFamilyIndices {
    graphics: u32,
    presentation: u32,
}

impl DeviceWrapper {
    #[allow(unused_unsafe)]
    pub unsafe fn new(
        instance: &ash::Instance,
        surface_entry: &khr::Surface,
        surface: vk::SurfaceKHR,
    ) -> VkResult<Self> {
        let (phys_device, queue_fam_indices) =
            unsafe { pick_physical(instance, surface_entry, surface)? };

        let logical_device = //`
            unsafe { create_logical(instance, phys_device, &queue_fam_indices)? };

        let graphics_queue =
            // NOTE: we pass 0 for queue index
            unsafe { logical_device.get_device_queue(queue_fam_indices.graphics, 0) };

        let present_queue =
            unsafe { logical_device.get_device_queue(queue_fam_indices.presentation, 0) };

        Ok(Self {
            logical: logical_device,
            physical: phys_device,
            queues: Queues {
                graphics: graphics_queue,
                graphics_family_index: queue_fam_indices.graphics,
                presentation: present_queue,
                present_family_index: queue_fam_indices.presentation,
            },
        })
    }

    pub unsafe fn destroy(&self) {
        self.logical.destroy_device(None);
    }
}

#[allow(unused_unsafe)]
unsafe fn pick_physical(
    instance: &ash::Instance,
    surface_entry: &khr::Surface,
    surface: vk::SurfaceKHR,
) -> VkResult<(vk::PhysicalDevice, QueueFamilyIndices)> {
    // entry to rest of function. calls the other subfunctions
    {
        return _pick_physical(instance, surface_entry, surface);
    }

    fn _pick_physical(
        instance: &ash::Instance,
        surface_entry: &khr::Surface,
        surface: vk::SurfaceKHR,
    ) -> VkResult<(vk::PhysicalDevice, QueueFamilyIndices)> {
        let phys_devices = unsafe { instance.enumerate_physical_devices()? };

        let mut preferred: Option<vk::PhysicalDevice> = None;
        let mut mem_largest = 0;
        let mut queue_fam_indices = QueueFamilyIndices {
            graphics: 0,
            presentation: 0,
        };

        for d in phys_devices {
            let props: vk::PhysicalDeviceProperties =
                unsafe { instance.get_physical_device_properties(d) };
            let mem_props: vk::PhysicalDeviceMemoryProperties =
                unsafe { instance.get_physical_device_memory_properties(d) };
            let queue_fam_props: Vec<vk::QueueFamilyProperties> =
                unsafe { instance.get_physical_device_queue_family_properties(d) };
            let exts = unsafe { instance.enumerate_device_extension_properties(d)? };

            queue_fam_indices =
                match _find_queue_fam_indices(d, &queue_fam_props, &surface_entry, surface)? {
                    Some(q) => q,
                    // require appropriate queue families
                    None => continue,
                };

            if _has_required_exts(exts) == false {
                // require appropriate extensions (only swapchain, as of now)
                continue;
            } else {
                // swapchain/surface support is present
                let _capabilities: vk::SurfaceCapabilitiesKHR =
                    unsafe { surface_entry.get_physical_device_surface_capabilities(d, surface)? };
                let formats: Vec<vk::SurfaceFormatKHR> =
                    unsafe { surface_entry.get_physical_device_surface_formats(d, surface)? };
                let present_modes: Vec<vk::PresentModeKHR> =
                    unsafe { surface_entry.get_physical_device_surface_present_modes(d, surface)? };

                // require more than 0 formats and present modes
                if formats.is_empty() || present_modes.is_empty() {
                    continue;
                }
            }

            // prefer discrete gpus
            if props.device_type == vk::PhysicalDeviceType::DISCRETE_GPU {
                preferred = Some(d);
                break;
            }

            // or pick the device with the most memory
            let mut mem_size = 0;
            for i in 0..mem_props.memory_heap_count {
                let heap = mem_props.memory_heaps[i as usize];
                if heap.flags.contains(vk::MemoryHeapFlags::DEVICE_LOCAL) {
                    mem_size += heap.size;
                }
            }
            if mem_size > mem_largest {
                mem_largest = mem_size;
                preferred = Some(d);
            }
        }

        match preferred {
            None => panic!(/* FIXME: handle err */),
            Some(pref) => return Ok((pref, queue_fam_indices)),
        }
    }

    // search for queue families supporting graphics and presentation
    // and return their indices (or None, if none are found)
    fn _find_queue_fam_indices(
        device: vk::PhysicalDevice,
        queue_fam_props: &Vec<vk::QueueFamilyProperties>,
        surface_entry: &khr::Surface,
        surface: vk::SurfaceKHR,
    ) -> VkResult<Option<QueueFamilyIndices>> {
        let mut graphics_ndx: Option<u32> = None;
        let mut present_ndx: Option<u32> = None;

        for (i, q) in queue_fam_props.iter().enumerate() {
            if q.queue_flags.contains(vk::QueueFlags::GRAPHICS) {
                graphics_ndx = Some(i as u32);
            }

            let supports_presentation = unsafe {
                surface_entry.get_physical_device_surface_support(device, i as u32, surface)?
            };

            if supports_presentation {
                present_ndx = Some(i as u32);
            }

            if let (Some(g), Some(p)) = (graphics_ndx, present_ndx) {
                return Ok(Some(QueueFamilyIndices {
                    graphics: g,
                    presentation: p,
                }));
            }
        }

        Ok(None)
    }

    // determine if device has the required
    // extensions (as given by device_extension_names())
    fn _has_required_exts(exts: Vec<vk::ExtensionProperties>) -> bool {
        for n in device_extension_names().iter() {
            let mut found = false;
            for ext in &exts {
                if util::strcmp(n, &ext.extension_name) {
                    found = true;
                    break;
                }
            }
            if found == false {
                return false;
            }
        }

        true
    }
}

// FIXME: unsafe?
#[allow(unused_unsafe)]
unsafe fn create_logical(
    instance: &ash::Instance,
    phys_device: vk::PhysicalDevice,
    queue_fam_indices: &QueueFamilyIndices,
) -> VkResult<ash::Device> {
    let graphics_queue_info = vk::DeviceQueueCreateInfo::builder()
        .queue_family_index(queue_fam_indices.graphics)
        .queue_priorities(&[1.0]);

    let present_queue_info = vk::DeviceQueueCreateInfo::builder()
        .queue_family_index(queue_fam_indices.presentation)
        .queue_priorities(&[1.0]);

    let queue_infos = [graphics_queue_info.build(), present_queue_info.build()];
    let ext_names = util::to_raw_ptrs(&device_extension_names());

    let mut create_info = if queue_fam_indices.graphics == queue_fam_indices.presentation {
        vk::DeviceCreateInfo::builder()
            // only pass one of the queue_create_infos
            // if their queue family indices are the same
            .queue_create_infos(&queue_infos[0..1])
            .enabled_extension_names(&ext_names)
    } else {
        vk::DeviceCreateInfo::builder()
            .queue_create_infos(&queue_infos)
            .enabled_extension_names(&ext_names)
        //  TODO: device features? if we require any specific ones
    };

    // validation layers
    let enabled_layer_names = util::to_raw_ptrs(&debug::validation_layer_names());

    if cfg!(debug_assertions) {
        create_info = create_info.enabled_layer_names(&enabled_layer_names)
    }

    Ok(unsafe { instance.create_device(phys_device, &create_info, None)? })
}

#[test]
fn device_extension_names_test() {
    assert_eq!(
        device_extension_names()[0].len(),
        khr::Swapchain::name().to_bytes_with_nul().len()
    );
}
