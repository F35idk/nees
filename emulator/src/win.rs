use pixel_renderer::xcb::{self, xproto};

pub struct XcbWindowWrapper {
    pub win: xcb::Window,
    pub connection: xcb::Connection,
    pub delete_reply: xproto::InternAtomReply,
}

impl XcbWindowWrapper {
    pub fn new(title: &str, width: u16, height: u16) -> Result<Self, xcb::ConnError> {
        let (connection, pref_screen) = xcb::Connection::connect(None)?;
        connection.has_error()?;

        let setup = connection.get_setup();
        let mut screen_iter = setup.roots();
        let screen = screen_iter.nth(pref_screen as usize).unwrap();
        let win = connection.generate_id();

        let value_list = [(
            xproto::CW_EVENT_MASK,
            xproto::EVENT_MASK_EXPOSURE
                | xproto::EVENT_MASK_KEY_PRESS
                | xproto::EVENT_MASK_KEY_RELEASE,
        )];

        xproto::create_window(
            &connection,
            xcb::COPY_FROM_PARENT as u8,
            win,
            screen.root(),
            0,
            0,
            width,
            height,
            5,
            xcb::WINDOW_CLASS_INPUT_OUTPUT as u16,
            screen.root_visual(),
            &value_list,
        );

        let wm_prot_cookie = xcb::intern_atom(&connection, true, "WM_PROTOCOLS");
        let del_window_cookie = xcb::intern_atom(&connection, false, "WM_DELETE_WINDOW");

        let wm_prot_reply = wm_prot_cookie.get_reply().unwrap();
        let del_window_reply = del_window_cookie.get_reply().unwrap();

        let del_window_atom = [del_window_reply.atom()];

        xcb::xproto::change_property(
            &connection,
            xcb::PROP_MODE_REPLACE as u8,
            win,
            wm_prot_reply.atom(),
            xcb::ATOM_ATOM,
            32,
            &del_window_atom,
        );

        xproto::change_property(
            &connection,
            xcb::PROP_MODE_REPLACE as u8,
            win,
            xcb::ATOM_WM_NAME,
            xcb::ATOM_STRING,
            8,
            title.as_bytes(),
        );

        Ok(Self {
            win,
            connection,
            delete_reply: del_window_reply,
        })
    }

    pub fn map_and_flush(&self) {
        xcb::map_window(&self.connection, self.win);
        self.connection.flush();
    }
}
