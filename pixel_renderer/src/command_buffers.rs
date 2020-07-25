use ash::prelude::VkResult;
use ash::version::DeviceV1_0;
use ash::{self, vk};

pub struct CommandBuffersWrapper {
    pub pool: vk::CommandPool,
    pub buffers: Vec<vk::CommandBuffer>,
    // TODO: separate transient_cmd_pool for short-lived commands
}

impl CommandBuffersWrapper {
    #[allow(unused_unsafe)]
    pub unsafe fn new(
        logical_device: &ash::Device,
        graphics_queue_family_index: u32,
        framebuffers: &Vec<vk::Framebuffer>,
        render_pass: vk::RenderPass,
        pipeline: vk::Pipeline,
        pipeline_layout: vk::PipelineLayout,
        descriptor_set: vk::DescriptorSet,
        swapchain_extent: vk::Extent2D,
        pixel_buffer: vk::Buffer,
        pixel_image: vk::Image,
        image_width: u32,
        image_height: u32,
        n_vertices: u32,
    ) -> VkResult<Self> {
        let main_pool_info =
            vk::CommandPoolCreateInfo::builder().queue_family_index(graphics_queue_family_index);
        //  TODO: specify flags to give vulkan hints as to how we will be using our command buffers

        let main_pool = unsafe { logical_device.create_command_pool(&main_pool_info, None)? };

        let cmd_buffers = unsafe {
            match Self::create_buffers(
                logical_device,
                main_pool,
                framebuffers,
                render_pass,
                pipeline,
                pipeline_layout,
                descriptor_set,
                swapchain_extent,
                pixel_buffer,
                pixel_image,
                image_width,
                image_height,
                n_vertices,
            ) {
                Ok(c) => c,
                Err(e) => {
                    logical_device.destroy_command_pool(main_pool, None);
                    return Err(e);
                }
            }
        };

        Ok(Self {
            buffers: cmd_buffers,
            pool: main_pool,
        })
    }

    // also called when recreating the swapchain during resize events
    #[allow(unused_unsafe)]
    pub unsafe fn create_buffers(
        logical_device: &ash::Device,
        command_pool: vk::CommandPool,
        framebuffers: &Vec<vk::Framebuffer>,
        render_pass: vk::RenderPass,
        pipeline: vk::Pipeline,
        pipeline_layout: vk::PipelineLayout,
        // currently we have just one descriptor set for our sampler descriptor
        descriptor_set: vk::DescriptorSet,
        swapchain_extent: vk::Extent2D,
        pixel_buffer: vk::Buffer,
        pixel_image: vk::Image,
        image_width: u32,
        image_height: u32,
        n_vertices: u32,
    ) -> VkResult<Vec<vk::CommandBuffer>> {
        // we need one command buffer for each framebuffer
        let cmd_buf_count = framebuffers.len();

        let cmd_buf_alloc_info = vk::CommandBufferAllocateInfo::builder()
            .command_pool(command_pool)
            // we currently have no use for secondary command buffers
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(cmd_buf_count as u32);

        // allocate command buffers
        let command_buffers =
            unsafe { logical_device.allocate_command_buffers(&cmd_buf_alloc_info)? };

        // record command buffers
        for (command_buffer, framebuffer) in command_buffers
            .iter() //'
            .zip(framebuffers.iter())
        {
            let cmd_begin_info = vk::CommandBufferBeginInfo::builder();
            // NOTE: could specify command buffer usage flags

            // begin recording
            unsafe { logical_device.begin_command_buffer(*command_buffer, &cmd_begin_info)? };

            // transition pixel_image to TRANSFER_DST_OPTIMAL layout for copying
            let copy_barrier = [vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::UNDEFINED)
                .new_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(pixel_image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: 1,
                })
                // no specific commands recorded prior to our
                // cmd_pipeline_barrier() call must be made available
                .src_access_mask(vk::AccessFlags::from_raw(0))
                // make the layout transition in our pipeline barrier visible to
                // any subsequent TRANSFER_WRITE commands (i.e our copy command)
                .dst_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                .build()];

            unsafe {
                logical_device.cmd_pipeline_barrier(
                    *command_buffer,
                    // start layout transition immediately
                    vk::PipelineStageFlags::TOP_OF_PIPE,
                    // block TRANSFER stages (our copy operation)
                    // until layout transition is complete
                    vk::PipelineStageFlags::TRANSFER,
                    vk::DependencyFlags::from_raw(0),
                    &[],
                    &[],
                    &copy_barrier,
                )
            };

            let copy_region = [vk::BufferImageCopy::builder()
                .buffer_offset(0)
                .buffer_row_length(0)
                .buffer_image_height(0)
                .buffer_image_height(0)
                .image_subresource(vk::ImageSubresourceLayers {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    mip_level: 0,
                    base_array_layer: 0,
                    layer_count: 1,
                })
                .image_offset(vk::Offset3D { x: 0, y: 0, z: 0 })
                .image_extent(vk::Extent3D {
                    width: image_width,
                    height: image_height,
                    depth: 1,
                })
                .build()];

            // copy pixel_buffer to pixel_image
            unsafe {
                logical_device.cmd_copy_buffer_to_image(
                    *command_buffer,
                    pixel_buffer,
                    pixel_image,
                    vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                    &copy_region,
                )
            }

            // transition pixel_image to SHADER_READ_ONLY_OPTIMAL layout
            let shader_read_barrier = [vk::ImageMemoryBarrier::builder()
                .old_layout(vk::ImageLayout::TRANSFER_DST_OPTIMAL)
                .new_layout(vk::ImageLayout::SHADER_READ_ONLY_OPTIMAL)
                .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
                .image(pixel_image)
                .subresource_range(vk::ImageSubresourceRange {
                    aspect_mask: vk::ImageAspectFlags::COLOR,
                    base_mip_level: 0,
                    level_count: 1,
                    base_array_layer: 0,
                    layer_count: 1,
                })
                // results of TRANSFER_WRITE operations should be made available
                .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
                // .. and visible to subsequent SHADER_READ accesses (we wish
                // to use our pixel_image as a texture in the fragment shader)
                .dst_access_mask(vk::AccessFlags::SHADER_READ)
                .build()];

            unsafe {
                logical_device.cmd_pipeline_barrier(
                    *command_buffer,
                    // start layout transition after commands in the
                    // TRANSFER stage (i.e after our buffer to image copy)
                    vk::PipelineStageFlags::TRANSFER,
                    // block fragment shader until layout transition is complete
                    vk::PipelineStageFlags::FRAGMENT_SHADER,
                    vk::DependencyFlags::from_raw(0),
                    &[],
                    &[],
                    &shader_read_barrier,
                )
            };

            let clear_color = [vk::ClearValue {
                color: vk::ClearColorValue {
                    float32: [0.0, 0.0, 0.0, 1.0],
                },
            }];

            let render_pass_begin_info = vk::RenderPassBeginInfo::builder()
                .render_pass(render_pass)
                .framebuffer(*framebuffer)
                // area shaders will affect
                .render_area(vk::Rect2D {
                    offset: vk::Offset2D { x: 0, y: 0 },
                    extent: swapchain_extent,
                })
                .clear_values(&clear_color);

            // begin render pass
            unsafe {
                logical_device.cmd_begin_render_pass(
                    *command_buffer,
                    &render_pass_begin_info,
                    // primary/secondary buffer stuff, we just use primary
                    vk::SubpassContents::INLINE,
                );
            }

            // bind graphics pipeline
            unsafe {
                logical_device.cmd_bind_pipeline(
                    *command_buffer,
                    vk::PipelineBindPoint::GRAPHICS,
                    pipeline,
                );
            }

            // bind descriptor sets
            unsafe {
                let desc_sets = [descriptor_set];
                logical_device.cmd_bind_descriptor_sets(
                    *command_buffer,
                    vk::PipelineBindPoint::GRAPHICS,
                    pipeline_layout,
                    0,
                    &desc_sets,
                    &[],
                );
            }

            // draw the fullscreen triangle
            unsafe {
                logical_device.cmd_draw(*command_buffer, n_vertices, 1, 0, 0);
            }

            // end render pass
            unsafe {
                logical_device.cmd_end_render_pass(*command_buffer);
            }

            // stop recording
            unsafe {
                logical_device.end_command_buffer(*command_buffer)?;
            }
        }

        Ok(command_buffers)
    }

    pub unsafe fn destroy(&self, logical_device: &ash::Device) {
        logical_device.destroy_command_pool(self.pool, None);
    }
}
