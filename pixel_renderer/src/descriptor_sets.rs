use ash::prelude::VkResult;
use ash::version::DeviceV1_0;
use ash::{self, vk};

// manages the resources that will be passed into shaders (except for
// the vertex buffer, which is its own object). as of writing this,
// DescriptorSetsWrapper is only needed for our combined image sampler
// NOTE: to future self: if we ever wish to be able to render multiple
// frames at once, we would have to allocate multiple descriptor sets and
// duplicate our descriptor resources (in this case our sampler and its
// texture) to ensure that each frame would have its own separate state
pub struct DescriptorSetsWrapper {
    pub pool: vk::DescriptorPool,
    pub sampler_layout: vk::DescriptorSetLayout,
    pub sampler_set: vk::DescriptorSet,
}

impl DescriptorSetsWrapper {
    #[allow(unused_unsafe)]
    pub unsafe fn new(
        img_view: vk::ImageView,
        sampler: vk::Sampler,
        logical_device: &ash::Device,
    ) -> VkResult<Self> {
        // create descriptor set layout for the sampler
        let sampler_layout = {
            let sampler_layout_binding = [vk::DescriptorSetLayoutBinding::builder()
                .binding(0)
                .descriptor_type(vk::DescriptorType::COMBINED_IMAGE_SAMPLER)
                .descriptor_count(1)
                .stage_flags(vk::ShaderStageFlags::FRAGMENT)
                .build()];
            //  OPTIMIZE: use immutable samplers?

            let sampler_layout_info =
                vk::DescriptorSetLayoutCreateInfo::builder().bindings(&sampler_layout_binding);

            [unsafe { logical_device.create_descriptor_set_layout(&sampler_layout_info, None)? }]
        };

        // create descriptor pool
        let desc_pool = {
            let pool_size = [vk::DescriptorPoolSize::builder()
                .ty(vk::DescriptorType::COMBINED_IMAGE_SAMPLER)
                // specifies how many descriptors of the above type that may be allocated
                // across all sets in the pool. we need just one for our sampler
                .descriptor_count(1)
                .build()];

            let pool_info = vk::DescriptorPoolCreateInfo::builder()
                .pool_sizes(&pool_size)
                .max_sets(1);

            unsafe {
                match logical_device.create_descriptor_pool(&pool_info, None) {
                    Ok(p) => p,
                    Err(e) => {
                        logical_device.destroy_descriptor_set_layout(sampler_layout[0], None);
                        return Err(e);
                    }
                }
            }
        };

        // allocate a single descriptor set for the sampler
        let sampler_desc_set = {
            let desc_set_alloc_info = vk::DescriptorSetAllocateInfo::builder()
                .descriptor_pool(desc_pool)
                .set_layouts(&sampler_layout);

            unsafe {
                match logical_device.allocate_descriptor_sets(&desc_set_alloc_info) {
                    Ok(s) => *s.first().unwrap(),
                    Err(e) => {
                        logical_device.destroy_descriptor_pool(desc_pool, None);
                        logical_device.destroy_descriptor_set_layout(sampler_layout[0], None);
                        return Err(e);
                    }
                }
            }
        };

        // update descriptor set to refer to the sampler and image view
        {
            let img_info = [vk::DescriptorImageInfo::builder()
                .image_layout(vk::ImageLayout::SHADER_READ_ONLY_OPTIMAL)
                .image_view(img_view)
                .sampler(sampler)
                .build()];

            let desc_set_write = [vk::WriteDescriptorSet::builder()
                .dst_set(sampler_desc_set)
                .dst_binding(0)
                .dst_array_element(0)
                .descriptor_type(vk::DescriptorType::COMBINED_IMAGE_SAMPLER)
                .image_info(&img_info)
                .build()];

            unsafe { logical_device.update_descriptor_sets(&desc_set_write, &[]) };
        }

        Ok(Self {
            pool: desc_pool,
            sampler_set: sampler_desc_set,
            sampler_layout: sampler_layout[0],
        })
    }

    pub unsafe fn destroy(&self, logical_device: &ash::Device) {
        logical_device.destroy_descriptor_pool(self.pool, None);
        logical_device.destroy_descriptor_set_layout(self.sampler_layout, None)
    }
}
