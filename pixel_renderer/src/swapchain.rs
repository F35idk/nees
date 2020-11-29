use ash::extensions::khr;
use ash::version::DeviceV1_0;
use ash::{self, vk};

use super::error::VulkanError;
use super::util;

pub struct SwapchainWrapper {
    pub entry: khr::Swapchain,
    pub swapchain: vk::SwapchainKHR,
    pub images: Vec<vk::Image>,
    pub image_views: Vec<vk::ImageView>,
    pub format: vk::Format,
    pub extent: vk::Extent2D,
}

impl SwapchainWrapper {
    #[allow(unused_unsafe)]
    pub unsafe fn new(
        instance: &ash::Instance,
        phys_device: vk::PhysicalDevice,
        logical_device: &ash::Device,
        graphics_queue_family_ndx: u32,
        present_queue_family_ndx: u32,
        surface_entry: &khr::Surface,
        surface: vk::SurfaceKHR,
        old_swapchain: Option<vk::SwapchainKHR>,
        preferred_present_mode: vk::PresentModeKHR,
        fallback_present_mode: Option<vk::PresentModeKHR>,
    ) -> Result<Self, VulkanError> {
        let queue_fam_indices = [present_queue_family_ndx, graphics_queue_family_ndx];
        let capabilities = unsafe {
            surface_entry.get_physical_device_surface_capabilities(phys_device, surface)?
        };
        let formats =
            unsafe { surface_entry.get_physical_device_surface_formats(phys_device, surface)? };
        let present_modes = unsafe {
            surface_entry.get_physical_device_surface_present_modes(phys_device, surface)?
        };

        let present_mode = match present_modes
            .into_iter()
            .find(|p| *p == preferred_present_mode)
        {
            Some(p) => p,
            None => {
                if let Some(f) = fallback_present_mode {
                    f
                } else {
                    return Err(VulkanError::NoPresentMode(
                        "Error while creating the swapchain: surface \
                         doesn't support the required present mode",
                    ));
                }
            }
        };

        let format = match formats
            .iter()
            .find(|f| f.format == vk::Format::B8G8R8A8_SRGB)
        {
            // if supported, prefer srgb
            Some(f) => f.format,
            // or fall back to first format given
            None => formats[0].format,
        };

        let img_count = if capabilities.max_image_count == 0 {
            // no specified max image count, default to min + 1
            capabilities.min_image_count + 1
        } else {
            capabilities.max_image_count
        };

        // no transformation
        let transform = capabilities.current_transform;
        // NOTE: since we're using xcb, the image
        // extent must always match the window size
        let extent = capabilities.current_extent;

        let mut create_info = vk::SwapchainCreateInfoKHR::builder()
            .surface(surface)
            .min_image_count(img_count)
            .image_format(format)
            .image_extent(extent)
            .image_array_layers(1)
            .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
            .present_mode(present_mode)
            .pre_transform(transform)
            // enable clipping
            .clipped(true)
            .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE);

        if graphics_queue_family_ndx != present_queue_family_ndx {
            // graphics and present queue families are different
            create_info = create_info
                // TODO: ideally, we should keep the sharing mode exclusive here as well
                .image_sharing_mode(vk::SharingMode::CONCURRENT)
                .queue_family_indices(&queue_fam_indices);
        } else {
            create_info = create_info //'
                .image_sharing_mode(vk::SharingMode::EXCLUSIVE);
        }

        // if an old swapchain was passed in
        if let Some(old) = old_swapchain {
            create_info = create_info.old_swapchain(old)
        }

        let entry = khr::Swapchain::new(instance, logical_device);
        let swapchain = unsafe { entry.create_swapchain(&create_info, None)? };
        let images = unsafe {
            match entry.get_swapchain_images(swapchain) {
                Ok(i) => i,
                Err(e) => {
                    entry.destroy_swapchain(swapchain, None);
                    return Err(VulkanError::Result(e));
                }
            }
        };

        let mut image_views = Vec::with_capacity(images.len());
        for img in &images {
            let view = match util::create_image_view(*img, format, logical_device) {
                Ok(v) => v,
                Err(e) => {
                    for v in image_views {
                        logical_device.destroy_image_view(v, None);
                    }

                    entry.destroy_swapchain(swapchain, None);
                    return Err(VulkanError::Result(e));
                }
            };

            image_views.push(view);
        }

        Ok(Self {
            entry,
            swapchain,
            images,
            image_views,
            format,
            extent,
        })
    }

    pub unsafe fn destroy(&self, logical_device: &ash::Device) {
        for img in &self.image_views {
            logical_device.destroy_image_view(*img, None);
        }

        self.entry.destroy_swapchain(self.swapchain, None);
    }
}
