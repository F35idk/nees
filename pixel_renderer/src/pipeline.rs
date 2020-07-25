#[macro_use]
use std;
use std::ffi::CStr;
use std::io::Cursor;

use ash::version::DeviceV1_0;
use ash::{self, vk};

use super::error::VulkanError;

pub struct PipelineWrapper {
    pub pipeline: vk::Pipeline,
    pub layout: vk::PipelineLayout,
    pub render_pass: vk::RenderPass,
}

impl PipelineWrapper {
    #[allow(unused_unsafe)]
    pub unsafe fn new(
        logical_device: &ash::Device,
        swapchain_extent: vk::Extent2D,
        swapchain_img_format: vk::Format,
        // currently we're just using a single descriptor
        // set layout for our sampler descriptor
        descriptor_set_layout: vk::DescriptorSetLayout,
    ) -> Result<PipelineWrapper, VulkanError> {
        let vert_module = unsafe {
            Self::create_shader_module(logical_device, include_bytes!("shaders/vert.spv"))?
        };
        let frag_module = unsafe {
            Self::create_shader_module(logical_device, include_bytes!("shaders/frag.spv"))?
        };
        let shader_entry_name = unsafe { CStr::from_bytes_with_nul_unchecked(b"main\0") };

        let vert_stage_info = vk::PipelineShaderStageCreateInfo::builder()
            .stage(vk::ShaderStageFlags::VERTEX)
            .module(vert_module)
            .name(shader_entry_name);

        let frag_stage_info = vk::PipelineShaderStageCreateInfo::builder()
            .stage(vk::ShaderStageFlags::FRAGMENT)
            .module(frag_module)
            .name(shader_entry_name);
        //  TODO: .specialization_info();

        let stage_infos = [vert_stage_info.build(), frag_stage_info.build()];

        // NOTE: if we were to use a vertex buffer, we would pass
        // in our attribute and binding descriptions here
        let vert_input_info = vk::PipelineVertexInputStateCreateInfo::builder();

        let input_asm = vk::PipelineInputAssemblyStateCreateInfo::builder()
            .topology(vk::PrimitiveTopology::TRIANGLE_LIST)
            .primitive_restart_enable(false);

        // viewport covers the entire framebuffer
        let viewport = [vk::Viewport::builder()
            .x(0.0)
            .y(0.0)
            .width(swapchain_extent.width as f32)
            .height(swapchain_extent.height as f32)
            .min_depth(0.0)
            .max_depth(1.0)
            .build()];

        // scissor covers the entire framebuffer
        let scissor = [vk::Rect2D::builder()
            .offset(vk::Offset2D { x: 0, y: 0 })
            .extent(swapchain_extent)
            .build()];

        let viewport_state = vk::PipelineViewportStateCreateInfo::builder()
            .viewports(&viewport)
            .scissors(&scissor);

        let rasterizer = vk::PipelineRasterizationStateCreateInfo::builder()
            .depth_clamp_enable(false)
            .rasterizer_discard_enable(false)
            .polygon_mode(vk::PolygonMode::FILL)
            .line_width(1.0)
            .cull_mode(vk::CullModeFlags::BACK)
            .front_face(vk::FrontFace::CLOCKWISE)
            .depth_bias_enable(false)
            .depth_bias_constant_factor(0.0)
            .depth_bias_clamp(0.0)
            .depth_bias_slope_factor(0.0);

        let multisample = vk::PipelineMultisampleStateCreateInfo::builder()
            .sample_shading_enable(false)
            .rasterization_samples(vk::SampleCountFlags::TYPE_1)
            // NOTE: rest are not actually needed
            .min_sample_shading(1.0)
            // .sample_mask()
            .alpha_to_coverage_enable(false)
            .alpha_to_one_enable(false);

        // NOTE: here we would initialize a PipelineDepthStencilStateCreateInfo

        // color blending per framebuffer
        let color_blend_attachment = [vk::PipelineColorBlendAttachmentState::builder()
            .color_write_mask(
                vk::ColorComponentFlags::R
                    | vk::ColorComponentFlags::G
                    | vk::ColorComponentFlags::B
                    | vk::ColorComponentFlags::A,
            )
            .blend_enable(false)
            // rest of the fields do not need to be specified as blend_enable
            // is false, but we keep them here for potential future changes
            .src_color_blend_factor(vk::BlendFactor::ONE)
            .dst_color_blend_factor(vk::BlendFactor::ZERO)
            .color_blend_op(vk::BlendOp::ADD)
            .src_alpha_blend_factor(vk::BlendFactor::ONE)
            .dst_alpha_blend_factor(vk::BlendFactor::ZERO)
            .alpha_blend_op(vk::BlendOp::ADD)
            .build()];

        // global color blending settings
        let color_blending = vk::PipelineColorBlendStateCreateInfo::builder()
            // we disable both logic_op and blend_enable, so our fragment colors
            // will be written directly to the framebuffer with no blending
            .logic_op_enable(false)
            .attachments(&color_blend_attachment);

        // NOTE: here we could initialize a PipelineDynanicStateCreateInfo to specify
        // things we want to be able to modify without recreating the entire pipeline

        let descriptor_set_layouts = [descriptor_set_layout];
        let pipeline_layout_info =
            vk::PipelineLayoutCreateInfo::builder().set_layouts(&descriptor_set_layouts);

        let pipeline_layout = unsafe {
            match logical_device.create_pipeline_layout(&pipeline_layout_info, None) {
                Ok(p) => p,
                Err(e) => {
                    logical_device.destroy_shader_module(vert_module, None);
                    logical_device.destroy_shader_module(frag_module, None);

                    return Err(VulkanError::Result(e));
                }
            }
        };

        let render_pass = {
            // the attachment descriptions array currently just contains our color attachment.
            // our fragment shader references the index of this attachment in the
            // attachment descriptions array (0)
            let color_attachment = [vk::AttachmentDescription::builder()
                .format(swapchain_img_format)
                .samples(vk::SampleCountFlags::TYPE_1)
                // we'll be redrawing the entire frame anyway, so
                // we don't care about its previous contents
                .load_op(vk::AttachmentLoadOp::DONT_CARE)
                .store_op(vk::AttachmentStoreOp::STORE)
                // don't care about stencil data
                .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
                .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
                // layout of attachment before render pass
                .initial_layout(vk::ImageLayout::UNDEFINED)
                // layout to transition to when render pass is done
                .final_layout(vk::ImageLayout::PRESENT_SRC_KHR)
                .build()];

            let color_attachment_ref = [vk::AttachmentReference::builder()
                .attachment(0)
                // layout we want to transition to in the subpass we create below
                .layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                .build()];

            let subpass = [vk::SubpassDescription::builder()
                .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
                .color_attachments(&color_attachment_ref)
                .build()];

            // we set up a subpass dependency to ensure correct behavior for the
            // 'external' subpass (other operations outside of the render pass).
            // basically, we want to ensure that image layout transitions do
            // not occur before the image is actually available
            // https://github.com/KhronosGroup/Vulkan-Docs/wiki/Synchronization-Examples#swapchain-image-acquire-and-present
            let dependency = [vk::SubpassDependency::builder()
                // external subpass
                .src_subpass(vk::SUBPASS_EXTERNAL)
                // index of our (only) subpass
                .dst_subpass(0)
                // by setting the external subpass's stage mask to color_attachment_ouptput,
                // we ensure that the transition does not happen before this stage
                .src_stage_mask(vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
                // the subpass that we set to wait on src (our subpass)
                // is also in the color attachment output stage
                .dst_stage_mask(vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
                // our subpass writes to the color attachment
                .dst_access_mask(vk::AccessFlags::COLOR_ATTACHMENT_WRITE)
                .build()];

            let render_pass_info = vk::RenderPassCreateInfo::builder()
                .attachments(&color_attachment)
                .subpasses(&subpass)
                .dependencies(&dependency);

            unsafe {
                match logical_device.create_render_pass(&render_pass_info, None) {
                    Ok(r) => r,
                    Err(e) => {
                        logical_device.destroy_shader_module(vert_module, None);
                        logical_device.destroy_shader_module(frag_module, None);
                        logical_device.destroy_pipeline_layout(pipeline_layout, None);

                        return Err(VulkanError::Result(e));
                    }
                }
            }
        };

        let pipeline_info = [vk::GraphicsPipelineCreateInfo::builder()
            .stages(&stage_infos)
            .vertex_input_state(&vert_input_info)
            .input_assembly_state(&input_asm)
            .viewport_state(&viewport_state)
            .rasterization_state(&rasterizer)
            .multisample_state(&multisample)
            .color_blend_state(&color_blending)
            .layout(pipeline_layout)
            .render_pass(render_pass)
            .subpass(0)
            .build()];

        let pipelines = unsafe {
            logical_device.create_graphics_pipelines(
                vk::PipelineCache::null(),
                &pipeline_info,
                None,
            )
        };

        unsafe {
            logical_device.destroy_shader_module(vert_module, None);
            logical_device.destroy_shader_module(frag_module, None);
        }

        let pipeline = match pipelines {
            // take the first pipeline in the pipelines vec (we only create one)
            Ok(p) => *p.first().unwrap(),
            Err((_, e)) => {
                logical_device.destroy_pipeline_layout(pipeline_layout, None);
                return Err(VulkanError::Result(e));
            }
        };

        Ok(PipelineWrapper {
            pipeline,
            layout: pipeline_layout,
            render_pass,
        })
    }

    #[allow(unused_unsafe)]
    unsafe fn create_shader_module(
        logical_device: &ash::Device,
        spirv: &[u8],
    ) -> Result<vk::ShaderModule, VulkanError> {
        let words = ash::util::read_spv(&mut Cursor::new(spirv))?;
        let module_info = vk::ShaderModuleCreateInfo::builder().code(&words);

        Ok(unsafe { logical_device.create_shader_module(&module_info, None)? })
    }

    pub unsafe fn destroy(&self, logical_device: &ash::Device) {
        logical_device.destroy_pipeline(self.pipeline, None);
        logical_device.destroy_pipeline_layout(self.layout, None);
        logical_device.destroy_render_pass(self.render_pass, None);
    }
}
