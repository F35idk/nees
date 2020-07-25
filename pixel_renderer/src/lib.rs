use std::ffi::CStr;

use ash::extensions::{ext, khr};
use ash::prelude::VkResult;
use ash::version::{DeviceV1_0, EntryV1_0, InstanceV1_0};
use ash::vk;

// re-export xcb and ash
pub use ash;
pub use xcb;
use xcb::base::Connection;
use xcb::xproto::Window;

#[macro_use]
mod debug;
mod buffer;
mod command_buffers;
mod descriptor_sets;
mod device;
mod error;
mod pipeline;
mod pixel_image;
mod surface;
mod swapchain;
mod util;

use command_buffers::CommandBuffersWrapper;
use debug::DebugMessengerWrapper;
use descriptor_sets::DescriptorSetsWrapper;
use device::DeviceWrapper;
use error::VulkanError;
use pipeline::PipelineWrapper;
use pixel_image::PixelImage;
use surface::SurfaceWrapper;
use swapchain::SwapchainWrapper;

// TODO: parameterize format and set pixel size based on it
const PIXEL_FORMAT: vk::Format = vk::Format::R8G8B8A8_UNORM;
const PIXEL_SIZE: u64 = 4;

#[cfg(debug_assertions)]
fn extension_names() -> Vec<*const i8> {
    vec![
        khr::Surface::name().as_ptr(),
        khr::XcbSurface::name().as_ptr(),
        ext::DebugUtils::name().as_ptr(),
    ]
}

#[cfg(not(debug_assertions))]
fn extension_names() -> Vec<*const i8> {
    vec![
        khr::Surface::name().as_ptr(),
        khr::XcbSurface::name().as_ptr(),
    ]
}

pub struct PixelRenderer {
    entry: ash::Entry,
    instance: ash::Instance,
    surface: SurfaceWrapper,
    device: DeviceWrapper,
    swapchain: SwapchainWrapper,

    pixel_image: PixelImage,
    sampler: vk::Sampler,
    descriptor_sets: DescriptorSetsWrapper,

    pipeline: PipelineWrapper,
    framebuffers: Vec<vk::Framebuffer>,
    command_buffers: CommandBuffersWrapper,

    image_available_semaphore: vk::Semaphore,

    debug_messenger: Option<DebugMessengerWrapper>,
}

impl PixelRenderer {
    pub fn new(
        xcb_conn: &mut Connection,
        xcb_win: Window,
        width: u64,
        height: u64,
    ) -> Result<Self, VulkanError> {
        let entry = ash::Entry::new()?;

        if cfg!(debug_assertions) {
            let available = entry
                .enumerate_instance_layer_properties()
                .expect("couldn't get required validation layers");

            for l in debug::validation_layer_names().iter() {
                let mut found = false;
                for a in &available {
                    if util::strcmp(l, &a.layer_name) {
                        found = true;
                        break;
                    }
                }

                if found == false {
                    panic!("required validation layers not available")
                }
            }
        }

        // set up ApplicationInfo
        let app_info = {
            let name: &'static CStr =
                unsafe { CStr::from_bytes_with_nul_unchecked(b"Hello Triangle\0") };

            // get system's vulkan api version
            let api_version = match entry.try_enumerate_instance_version()? {
                Some(_) => vk::make_version(1, 1, 0),
                None => vk::make_version(1, 0, 0),
            };

            vk::ApplicationInfo::builder()
                .application_name(name)
                .application_version(0)
                .engine_name(name)
                .engine_version(0)
                // we cannot use features newer than what is indicated
                // by this field (per the vulkan manual).
                .api_version(api_version)
        };

        // set up InstanceCreateInfo
        let ext_names = extension_names();
        let mut dbg_msg_info = DebugMessengerWrapper::get_create_info();
        let enabled_layer_names = util::to_raw_ptrs(&debug::validation_layer_names());

        let instance_info = if cfg!(debug_assertions) {
            vk::InstanceCreateInfo::builder()
                .application_info(&app_info)
                .enabled_extension_names(&ext_names)
                .enabled_layer_names(&enabled_layer_names)
                .push_next(&mut dbg_msg_info)
        } else {
            vk::InstanceCreateInfo::builder()
                .application_info(&app_info)
                .enabled_extension_names(&ext_names)
        };

        // mustn't outlast ash::Entry, hence the unsafe
        let instance = unsafe { entry.create_instance(&instance_info, None)? };

        // set up surface (currently only xcb supported)
        let surface = unsafe {
            match SurfaceWrapper::new_xcb(&entry, &instance, xcb_conn, xcb_win) {
                Ok(o) => o,
                Err(e) => {
                    // manually destroy allocated vulkan objects
                    // before propagating error to the caller
                    instance.destroy_instance(None);
                    return Err(VulkanError::Result(e));
                }
            }
        };

        let device = unsafe {
            match DeviceWrapper::new(&instance, &surface.entry, surface.surface) {
                Ok(o) => o,
                Err(e) => {
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        let swapchain = unsafe {
            match SwapchainWrapper::new(
                &instance,
                device.physical,
                &device.logical,
                device.queues.graphics_family_index,
                device.queues.present_family_index,
                &surface.entry,
                surface.surface,
                None, // no old swapchain
                // TODO: consider parameterizing present mode
                vk::PresentModeKHR::IMMEDIATE,
                Some(vk::PresentModeKHR::FIFO),
            ) {
                Ok(o) => o,
                Err(e) => {
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(e);
                }
            }
        };

        let pixel_image = unsafe {
            match PixelImage::new(
                &instance, //`
                &device.logical,
                device.physical,
                width,
                height,
                PIXEL_FORMAT,
                PIXEL_SIZE,
            ) {
                Ok(o) => o,
                Err(e) => {
                    swapchain.destroy(&device.logical);
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(e);
                }
            }
        };

        let sampler = {
            let sampler_info = vk::SamplerCreateInfo::builder()
                .mag_filter(vk::Filter::NEAREST)
                .min_filter(vk::Filter::NEAREST)
                // address modes don't matter for our fullscreen triangle
                .address_mode_u(vk::SamplerAddressMode::CLAMP_TO_BORDER)
                .address_mode_v(vk::SamplerAddressMode::CLAMP_TO_BORDER)
                .address_mode_w(vk::SamplerAddressMode::CLAMP_TO_BORDER)
                .anisotropy_enable(false)
                .border_color(vk::BorderColor::INT_OPAQUE_BLACK)
                .unnormalized_coordinates(false)
                .compare_enable(false);

            unsafe {
                match device.logical.create_sampler(&sampler_info, None) {
                    Ok(o) => o,
                    Err(e) => {
                        pixel_image.destroy(&device.logical);
                        swapchain.destroy(&device.logical);
                        device.destroy();
                        surface.destroy();
                        instance.destroy_instance(None);

                        return Err(VulkanError::Result(e));
                    }
                }
            }
        };

        let descriptor_sets = unsafe {
            match DescriptorSetsWrapper::new(pixel_image.view, sampler, &device.logical) {
                Ok(o) => o,
                Err(e) => {
                    device.logical.destroy_sampler(sampler, None);
                    pixel_image.destroy(&device.logical);
                    swapchain.destroy(&device.logical);
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        let pipeline = unsafe {
            match PipelineWrapper::new(
                &device.logical,
                swapchain.extent,
                swapchain.format,
                descriptor_sets.sampler_layout,
            ) {
                Ok(o) => o,
                Err(e) => {
                    descriptor_sets.destroy(&device.logical);
                    device.logical.destroy_sampler(sampler, None);
                    pixel_image.destroy(&device.logical);
                    swapchain.destroy(&device.logical);
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(e);
                }
            }
        };

        let framebuffers = unsafe {
            match Self::create_framebuffers(
                &device.logical,
                &swapchain.image_views,
                pipeline.render_pass,
                swapchain.extent,
            ) {
                Ok(o) => o,
                Err(e) => {
                    pipeline.destroy(&device.logical);
                    descriptor_sets.destroy(&device.logical);
                    device.logical.destroy_sampler(sampler, None);
                    pixel_image.destroy(&device.logical);
                    swapchain.destroy(&device.logical);
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        let command_buffers = unsafe {
            match CommandBuffersWrapper::new(
                &device.logical,
                device.queues.graphics_family_index,
                &framebuffers,
                pipeline.render_pass,
                pipeline.pipeline,
                pipeline.layout,
                descriptor_sets.sampler_set,
                swapchain.extent,
                pixel_image.buffer.buffer,
                pixel_image.image,
                pixel_image.width as u32,
                pixel_image.height as u32,
                3, // 3 vertices to draw
            ) {
                Ok(o) => o,
                Err(e) => {
                    for f in &framebuffers {
                        device.logical.destroy_framebuffer(*f, None);
                    }

                    pipeline.destroy(&device.logical);
                    descriptor_sets.destroy(&device.logical);
                    device.logical.destroy_sampler(sampler, None);
                    pixel_image.destroy(&device.logical);
                    swapchain.destroy(&device.logical);
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        // set up semaphores
        let semaphore_info = vk::SemaphoreCreateInfo::builder();
        let image_available_semaphore = unsafe {
            match device.logical.create_semaphore(&semaphore_info, None) {
                Ok(o) => o,
                Err(e) => {
                    command_buffers.destroy(&device.logical);
                    for f in &framebuffers {
                        device.logical.destroy_framebuffer(*f, None);
                    }

                    pipeline.destroy(&device.logical);
                    descriptor_sets.destroy(&device.logical);
                    device.logical.destroy_sampler(sampler, None);
                    pixel_image.destroy(&device.logical);
                    swapchain.destroy(&device.logical);
                    device.destroy();
                    surface.destroy();
                    instance.destroy_instance(None);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        let debug_messenger = if cfg!(debug_assertions) {
            Some(unsafe {
                match DebugMessengerWrapper::new(&entry, &instance) {
                    Ok(o) => o,
                    Err(e) => {
                        device
                            .logical
                            .destroy_semaphore(image_available_semaphore, None);

                        command_buffers.destroy(&device.logical);
                        for f in &framebuffers {
                            device.logical.destroy_framebuffer(*f, None);
                        }

                        pipeline.destroy(&device.logical);
                        descriptor_sets.destroy(&device.logical);
                        device.logical.destroy_sampler(sampler, None);
                        pixel_image.destroy(&device.logical);
                        swapchain.destroy(&device.logical);
                        device.destroy();
                        surface.destroy();
                        instance.destroy_instance(None);

                        return Err(VulkanError::Result(e));
                    }
                }
            })
        } else {
            None
        };

        Ok(PixelRenderer {
            entry,
            instance,
            surface,
            device,
            swapchain,
            pipeline,
            pixel_image,
            sampler,
            descriptor_sets,
            framebuffers,
            command_buffers,
            image_available_semaphore,
            debug_messenger,
        })
    }

    #[inline]
    pub fn get_pixels<'a>(&'a mut self) -> &'a mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut::<u8>(
                self.pixel_image.pixels_raw,
                (self.pixel_image.width * self.pixel_image.height * PIXEL_SIZE) as usize,
            )
        }
    }

    pub fn get_dimensions_bytes(&self) -> (u64, u64) {
        (
            self.pixel_image.width * PIXEL_SIZE,
            self.pixel_image.height * PIXEL_SIZE,
        )
    }

    // renders a frame without presenting it. returns the index of the image rendered to
    pub fn render_frame(&mut self) -> u32 {
        // get index of next image in swapchain
        // NOTE: this operation is asynchronous and we use a semaphore
        // that signals us when the index can actually be used
        let image_index = match unsafe {
            self.swapchain.entry.acquire_next_image(
                self.swapchain.swapchain,
                std::u64::MAX, // no timeout
                self.image_available_semaphore,
                vk::Fence::null(), // no fence
            )
        } {
            Err(e) => {
                if e == vk::Result::ERROR_OUT_OF_DATE_KHR {
                    unsafe {
                        self.recreate_swapchain()
                            .expect("failed to recreate swapchain in render_frame()");
                    }

                    return self.render_frame(); // try again
                } else {
                    panic!(
                        "error when acquiring image from swapchain in render_frame(): '{}'",
                        e
                    );
                }
            }
            Ok((index, suboptimal)) => {
                // if swapchain doesn't match surface exactly (due to resize)
                if suboptimal == true {
                    // TODO: we may wish to handle this as well
                }
                index
            }
        };

        // our commands need to wait for the next image to be available before drawing on it
        let wait_semaphore = [self.image_available_semaphore];
        // i.e if we reach the 'COLOR_ATTACHMENT_OUTPUT' stage in our pipeline early,
        // we need to wait on the signal from the semaphore. so this is our wait stage
        let wait_stage = [vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT];
        // we want to submit the command buffer that will start a render pass
        // on the framebuffer that holds the image we retrieved (as a color
        // attachment). the index of this command buffer is the same as the
        // index of the image it corresponds to
        let cmd_buf_to_submit = [self.command_buffers.buffers[image_index as usize]];
        // we don't need any signal semaphores, as render_frame() waits
        // on all graphics queue operations to finish before returning,
        // meaning PixelRenderer::present() won't ever be called prematurely
        let signal_semaphores = [];

        let submit_info = [vk::SubmitInfo::builder()
            .wait_semaphores(&wait_semaphore)
            .wait_dst_stage_mask(&wait_stage)
            .command_buffers(&cmd_buf_to_submit)
            .signal_semaphores(&signal_semaphores)
            .build()];

        // flush potential writes to the pixel buffer mapped memory
        unsafe {
            self.pixel_image
                .flush_mapped_buffer(&self.device.logical)
                .expect("failed to flush mapped memory for pixel buffer in render_frame()");
        }

        // submit command buffer to queue
        unsafe {
            self.device
                .logical
                .queue_submit(
                    self.device.queues.graphics,
                    &submit_info,
                    // no fence
                    vk::Fence::null(),
                )
                .expect("failed to submit command buffer to queue in render_frame()");
        }

        // wait for submitted operations to finish
        unsafe {
            self.device
                .logical
                .queue_wait_idle(self.device.queues.graphics)
                .expect("failed to wait for queue operations to finish in render_frame()")
        };

        image_index

        // OPTIMIZE: instead of waiting for all operations to finish, we could set up a fence to
        // only wait for our buffer copy operation to finish, and then yield from render_frame()
        // when that is done. this way we could allow more work to be done on the cpu before the
        // next call to render_frame() and subsequent command buffer submission (where we would
        // probably have to wait for previous submits anyway, but the waiting would at least have
        // been delayed until all necessary cpu work has completed). doing this could help minimize
        // waiting and maximize cpu work per frame. note that this would also require some changes
        // to present()
    }

    // presents the image corresponding to image_index.
    pub fn present(&mut self, image_index: u32) {
        let swapchains = [self.swapchain.swapchain];
        let img_indices = [image_index];
        let present_info = vk::PresentInfoKHR::builder()
            // no wait semaphores needed
            .wait_semaphores(&[])
            .swapchains(&swapchains)
            .image_indices(&img_indices);

        // queue image for presentation
        unsafe {
            if let Err(e) = self
                .swapchain
                .entry
                .queue_present(self.device.queues.presentation, &present_info)
            {
                // handle swapchain not matching surface (due to window resize)
                if e == vk::Result::ERROR_OUT_OF_DATE_KHR || e == vk::Result::SUBOPTIMAL_KHR {
                    self.recreate_swapchain()
                        .expect("failed to recreate swapchain in render_frame()");
                }
            }
        }

        // wait for presentation to finish
        unsafe {
            self.device
                .logical
                .queue_wait_idle(self.device.queues.presentation)
                .expect("failed to wait for presentation to finish in present()")
        };
    }

    #[allow(unused_unsafe)]
    unsafe fn create_framebuffers(
        logical_device: &ash::Device,
        image_views: &Vec<vk::ImageView>,
        render_pass: vk::RenderPass,
        swapchain_extent: vk::Extent2D,
    ) -> VkResult<Vec<vk::Framebuffer>> {
        // we need one framebuffer per swapchain image view
        image_views
            .iter()
            .map(|view| {
                let attachments = [*view];

                let framebuffer_info = vk::FramebufferCreateInfo::builder()
                    .render_pass(render_pass)
                    .attachments(&attachments)
                    .width(swapchain_extent.width)
                    .height(swapchain_extent.height)
                    .layers(1);

                unsafe { logical_device.create_framebuffer(&framebuffer_info, None) }
            })
            .collect::<VkResult<Vec<vk::Framebuffer>>>()
    }

    // called on window resize to ensure the swapchain is still valid
    #[allow(unused_unsafe)]
    unsafe fn recreate_swapchain(&mut self) -> Result<(), VulkanError> {
        // create new swapchain-dependent objects
        let new_swapchain = unsafe {
            SwapchainWrapper::new(
                &self.instance,
                self.device.physical,
                &self.device.logical,
                self.device.queues.graphics_family_index,
                self.device.queues.present_family_index,
                &self.surface.entry,
                self.surface.surface,
                // pass current swapchain as old_swapchain
                Some(self.swapchain.swapchain),
                vk::PresentModeKHR::IMMEDIATE,
                Some(vk::PresentModeKHR::FIFO),
            )?
        };

        let new_pipeline = unsafe {
            match PipelineWrapper::new(
                &self.device.logical,
                new_swapchain.extent,
                new_swapchain.format,
                self.descriptor_sets.sampler_layout,
            ) {
                Ok(o) => o,
                Err(e) => {
                    new_swapchain.destroy(&self.device.logical);
                    return Err(e);
                }
            }
        };

        // NOTE: anything that depends on the amount of
        // images the swapchain holds has to be recreated
        let new_framebuffers = unsafe {
            match Self::create_framebuffers(
                &self.device.logical,
                &new_swapchain.image_views,
                new_pipeline.render_pass,
                new_swapchain.extent,
            ) {
                Ok(o) => o,
                Err(e) => {
                    new_swapchain.destroy(&self.device.logical);
                    new_pipeline.destroy(&self.device.logical);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        let new_command_buffers = unsafe {
            match CommandBuffersWrapper::create_buffers(
                &self.device.logical,
                self.command_buffers.pool,
                &new_framebuffers,
                new_pipeline.render_pass,
                new_pipeline.pipeline,
                new_pipeline.layout,
                self.descriptor_sets.sampler_set,
                new_swapchain.extent,
                self.pixel_image.buffer.buffer,
                self.pixel_image.image,
                self.pixel_image.width as u32,
                self.pixel_image.height as u32,
                3,
            ) {
                Ok(o) => o,
                Err(e) => {
                    for f in new_framebuffers {
                        self.device.logical.destroy_framebuffer(f, None);
                    }

                    new_swapchain.destroy(&self.device.logical);
                    new_pipeline.destroy(&self.device.logical);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        // wait for all queue operations to finish
        unsafe {
            match self.device.logical.device_wait_idle() {
                Ok(o) => o,
                Err(e) => {
                    self.device
                        .logical
                        .free_command_buffers(self.command_buffers.pool, &new_command_buffers);

                    for f in new_framebuffers {
                        self.device.logical.destroy_framebuffer(f, None);
                    }

                    new_swapchain.destroy(&self.device.logical);
                    new_pipeline.destroy(&self.device.logical);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        // destroy current swapchain-dependent objects
        unsafe {
            for f in &self.framebuffers {
                self.device.logical.destroy_framebuffer(*f, None);
            }

            self.swapchain.destroy(&self.device.logical);
            self.pipeline.destroy(&self.device.logical);

            // free command buffers instead of destroying entire pool
            self.device
                .logical
                .free_command_buffers(self.command_buffers.pool, &self.command_buffers.buffers);
        }

        // add new swapchain objects to self
        self.pipeline = new_pipeline;
        self.swapchain = new_swapchain;
        self.framebuffers = new_framebuffers;
        self.command_buffers.buffers = new_command_buffers;

        Ok(())
    }
}

impl Drop for PixelRenderer {
    fn drop(&mut self) {
        unsafe {
            // wait for all queue operations to finish before destroying
            self.device
                .logical
                .device_wait_idle()
                .expect("call to device_wait_idle() failed when dropping PixelRenderer");

            if let Some(m) = &mut self.debug_messenger {
                m.destroy();
            }

            self.device
                .logical
                .destroy_semaphore(self.image_available_semaphore, None);

            self.command_buffers.destroy(&self.device.logical);

            for f in &self.framebuffers {
                self.device.logical.destroy_framebuffer(*f, None);
            }

            self.pipeline.destroy(&self.device.logical);
            self.descriptor_sets.destroy(&self.device.logical);
            self.device.logical.destroy_sampler(self.sampler, None);
            self.pixel_image.destroy(&self.device.logical);
            // destroy swapchain before device and surface
            self.swapchain.destroy(&self.device.logical);
            self.device.destroy();
            self.surface.destroy();

            self.instance.destroy_instance(None);
        }
    }
}
