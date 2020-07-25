macro_rules! logln {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { println!( $( $args ),* ); } }
}

macro_rules! log {
    ($( $args:expr ),*) => { if cfg!(feature = "logging") { print!( $( $args ),* ); } }
}
