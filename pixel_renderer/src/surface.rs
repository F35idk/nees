use ash;
use ash::extensions::khr;
use ash::prelude::VkResult;
use ash::vk;

use xcb::base::Connection;
use xcb::xproto::Window;

pub struct SurfaceWrapper<'a> {
    pub entry: khr::Surface,
    pub surface: vk::SurfaceKHR,
    xcb_conn: std::marker::PhantomData<&'a Connection>,
}

impl<'a> SurfaceWrapper<'a> {
    pub unsafe fn new_xcb(
        entry: &ash::Entry,
        instance: &ash::Instance,
        xcb_conn: &'a Connection,
        xcb_win: Window,
    ) -> VkResult<Self> {
        let surface_ext_entry = khr::Surface::new(entry, instance);
        let xcb_surface_entry = khr::XcbSurface::new(entry, instance);
        let create_info = vk::XcbSurfaceCreateInfoKHR::builder()
            // cast conn to void ptr
            .connection(xcb_conn.get_raw_conn() as *mut _)
            .window(xcb_win);

        let surface = xcb_surface_entry.create_xcb_surface(&create_info, None)?;

        // NOTE: we discard the xcb_surface_entry (the extension loader object)
        Ok(Self {
            entry: surface_ext_entry,
            surface,
            xcb_conn: std::marker::PhantomData,
        })
    }

    pub unsafe fn destroy(&self) {
        self.entry.destroy_surface(self.surface, None);
    }
}
