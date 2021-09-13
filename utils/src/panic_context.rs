use std::{cell::RefCell, panic, sync::Once};

#[macro_export]
macro_rules! panic_ctx {
    ($s:expr) => {{
        if cfg!(debug_assertions) {
            Some($crate::panic_context::new($s))
        } else {
            None
        }
    }};
}

#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn new(_: String) -> () {}

#[cfg(debug_assertions)]
pub fn new(context: String) -> PanicContext {
    static ONCE: Once = Once::new();
    ONCE.call_once(PanicContext::init);

    with_ctx(|ctx| ctx.push(context));
    PanicContext { _priv: () }
}

#[must_use]
#[cfg(debug_assertions)]
pub struct PanicContext {
    _priv: (),
}

#[cfg(debug_assertions)]
impl PanicContext {
    fn init() {
        let default_hook = panic::take_hook();
        let hook = move |panic_info: &panic::PanicInfo<'_>| {
            with_ctx(|ctx| {
                if !ctx.is_empty() {
                    eprintln!("Panic context:");
                    for frame in ctx.iter() {
                        eprintln!("> {}\n", frame)
                    }
                }
                default_hook(panic_info)
            })
        };
        panic::set_hook(Box::new(hook))
    }
}

#[cfg(debug_assertions)]
impl Drop for PanicContext {
    fn drop(&mut self) {
        with_ctx(|ctx| assert!(ctx.pop().is_some()))
    }
}

#[cfg(debug_assertions)]
fn with_ctx(f: impl FnOnce(&mut Vec<String>)) {
    thread_local! {
        static CTX: RefCell<Vec<String>> = RefCell::new(Vec::new());
    }
    CTX.with(|ctx| f(&mut *ctx.borrow_mut()))
}
