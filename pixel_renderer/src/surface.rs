use ash;
use ash::extensions::khr;
use ash::prelude::VkResult;
use ash::vk;

use xcb::base::Connection;
use xcb::xproto::Window;

pub struct SurfaceWrapper {
    pub entry: khr::Surface,
    pub surface: vk::SurfaceKHR,
}

impl SurfaceWrapper {
    pub unsafe fn new_xcb(
        entry: &ash::Entry,
        instance: &ash::Instance,
        xcb_conn: &mut Connection,
        xcb_win: Window,
    ) -> VkResult<Self> {
        let surface_ext_entry = khr::Surface::new(entry, instance);
        let xcb_surface_entry = khr::XcbSurface::new(entry, instance);
        let create_info = vk::XcbSurfaceCreateInfoKHR::builder()
            // cast conn to void ptr
            .connection(std::mem::transmute(xcb_conn.get_raw_conn()))
            .window(xcb_win);

        let surface = xcb_surface_entry.create_xcb_surface(&create_info, None)?;

        // NOTE: we discard the xcb_surface_entry (the extension loader object)
        Ok(Self {
            entry: surface_ext_entry,
            surface,
        })
    }

    pub unsafe fn destroy(&self) {
        self.entry.destroy_surface(self.surface, None);
    }
}
