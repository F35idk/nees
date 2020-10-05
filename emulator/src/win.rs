use super::xcb;

pub enum Keys {}

// constants for x11 keysym values
impl Keys {
    pub const SPACE: u32 = 0x20;
    pub const ESC: u32 = 0xff1b;
    pub const TAB: u32 = 0xff09;
    pub const SHIFT: u32 = 0xffe1;
    pub const W: u32 = 0x77;
    pub const A: u32 = 0x61;
    pub const S: u32 = 0x73;
    pub const D: u32 = 0x64;
    pub const F: u32 = 0x66;
    pub const R: u32 = 0x72;
}

pub struct XcbWindowWrapper {
    pub win: xcb::Window,
    pub connection: xcb::Connection,
    pub delete_reply: xcb::InternAtomReply,
    pub events: EventStore,
}

#[derive(Default)]
// struct needed for ignoring auto-repeated key release events. by storing the next event
// in the 'next' field, the struct allows 'peeking' into the next event without losing
// access to it in the process. xcb's internal event queue lacks such a 'peek' function,
// meaning any reads from it will automatically dequeue the event that was read
pub struct EventStore {
    pub curr: Option<xcb::GenericEvent>,
    pub next: Option<xcb::GenericEvent>,
}

impl EventStore {
    pub fn update(&mut self, new_event: Option<xcb::GenericEvent>) {
        // drop the original 'curr' and replace it with 'next' while replacing 'next' with 'new_event'
        self.curr = std::mem::replace(&mut self.next, new_event);
    }

    pub fn get_current(&mut self) -> &Option<xcb::GenericEvent> {
        if let Some(current) = self
            .curr
            .as_ref()
            .filter(|c| (c.response_type() & !0x80) == xcb::KEY_RELEASE)
        {
            if let Some(next) = self
                .next
                .as_ref()
                .filter(|c| (c.response_type() & !0x80) == xcb::KEY_PRESS)
            {
                let key_release: &xcb::KeyReleaseEvent = unsafe { xcb::cast_event(&current) };
                let next_key_press: &xcb::KeyPressEvent = unsafe { xcb::cast_event(&next) };

                if key_release.time() == next_key_press.time() {
                    // ignore key release event if next event is a key press that occured
                    // at the exact same time (this means autorepeat has kicked in)
                    return &None;
                }
            }
        }

        &self.curr
    }
}

impl XcbWindowWrapper {
    pub fn new(title: &str, width: u16, height: u16) -> Result<Self, xcb::ConnError> {
        let (connection, pref_screen) = xcb::Connection::connect(None)?;
        connection.has_error()?;

        let setup = connection.get_setup();
        let mut screen_iter = setup.roots();
        let screen = screen_iter.nth(pref_screen as usize).unwrap();
        let win = connection.generate_id();

        let value_list = [
            (xcb::CW_BACK_PIXEL, screen.white_pixel()),
            (
                xcb::CW_EVENT_MASK,
                xcb::EVENT_MASK_KEY_PRESS | xcb::EVENT_MASK_KEY_RELEASE,
            ),
        ];

        xcb::create_window(
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

        xcb::change_property(
            &connection,
            xcb::PROP_MODE_REPLACE as u8,
            win,
            wm_prot_reply.atom(),
            xcb::ATOM_ATOM,
            32,
            &del_window_atom,
        );

        xcb::change_property(
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
            events: EventStore::default(),
        })
    }

    pub fn map_and_flush(&self) {
        xcb::map_window(&self.connection, self.win);
        self.connection.flush();
    }
}
