use std::ffi::c_void;
use std::ffi::CStr;

use ash::extensions::ext;
use ash::{self, vk};

// validation layers to enable for debugging
pub fn validation_layer_names() -> [&'static [i8]; 1] {
    [unsafe { &*(b"VK_LAYER_KHRONOS_validation\0" as *const [u8] as *const [i8]) }]
}

// holds our debug messenger object along with
// the entry into the DebugUtils extension
#[cfg(debug_assertions)]
pub struct DebugMessengerWrapper {
    entry: ext::DebugUtils,
    messenger: vk::DebugUtilsMessengerEXT,
}

#[cfg(debug_assertions)]
impl DebugMessengerWrapper {
    pub fn new(entry: &ash::Entry, instance: &ash::Instance) -> Result<Self, vk::Result> {
        let debug_utils = ext::DebugUtils::new(entry, instance);
        let create_info = Self::get_create_info();
        let debug_messenger =
            unsafe { debug_utils.create_debug_utils_messenger(&create_info, None)? };

        Ok(Self {
            entry: debug_utils,
            messenger: debug_messenger,
        })
    }

    pub fn get_create_info<'a>() -> vk::DebugUtilsMessengerCreateInfoEXTBuilder<'a> {
        vk::DebugUtilsMessengerCreateInfoEXT::builder()
            // NOTE: we're skipping the 'INFO' severity flag
            // TODO: parameterize these?
            .message_severity(
                vk::DebugUtilsMessageSeverityFlagsEXT::VERBOSE
                    | vk::DebugUtilsMessageSeverityFlagsEXT::WARNING
                    | vk::DebugUtilsMessageSeverityFlagsEXT::ERROR,
            )
            .message_type(vk::DebugUtilsMessageTypeFlagsEXT::all())
            .pfn_user_callback(Some(Self::debug_callback))
    }

    // callback to handle debug messages
    unsafe extern "system" fn debug_callback(
        msg_severity: vk::DebugUtilsMessageSeverityFlagsEXT,
        msg_types: vk::DebugUtilsMessageTypeFlagsEXT,
        p_cb_data: *const vk::DebugUtilsMessengerCallbackDataEXT,
        _: *mut c_void,
    ) -> vk::Bool32 {
        // SAFETY: panic=abort is set for both dev and release profiles, so println!()
        // and CStr::from_ptr() won't ever cause unwinds (which is unsafe in ffi code)
        println!(
            "severity: '{:?}',\ntypes: '{:?}',\nmsg: {:?}",
            msg_severity,
            msg_types,
            CStr::from_ptr((*p_cb_data).p_message)
        );

        vk::FALSE
    }

    pub unsafe fn destroy(&self) {
        self.entry
            .destroy_debug_utils_messenger(self.messenger, None);
    }
}

#[cfg(not(debug_assertions))]
pub struct DebugMessengerWrapper {}

#[cfg(not(debug_assertions))]
impl DebugMessengerWrapper {
    pub fn new(_: &ash::Entry, _: &ash::Instance) -> Result<Self, vk::Result> {
        Ok(Self {})
    }

    pub fn get_create_info<'a>() -> vk::DebugUtilsMessengerCreateInfoEXTBuilder<'a> {
        vk::DebugUtilsMessengerCreateInfoEXT::builder()
            .message_severity(
                vk::DebugUtilsMessageSeverityFlagsEXT::VERBOSE
                    | vk::DebugUtilsMessageSeverityFlagsEXT::WARNING
                    | vk::DebugUtilsMessageSeverityFlagsEXT::ERROR,
            )
            .message_type(vk::DebugUtilsMessageTypeFlagsEXT::all())
            .pfn_user_callback(Some(Self::debug_callback))
    }

    unsafe extern "system" fn debug_callback(
        _: vk::DebugUtilsMessageSeverityFlagsEXT,
        _: vk::DebugUtilsMessageTypeFlagsEXT,
        _: *const vk::DebugUtilsMessengerCallbackDataEXT,
        _: *mut c_void,
    ) -> vk::Bool32 {
        vk::FALSE
    }

    pub unsafe fn destroy(&self) {}
}

#[test]
fn validation_layer_names_test() {
    assert_eq!(
        validation_layer_names()[0].len(),
        b"VK_LAYER_KHRONOS_validation\0".len()
    );
}
