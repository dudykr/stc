#![recursion_limit = "2048"]

#[macro_use]
extern crate napi_derive;

use backtrace::Backtrace;
use napi::CallContext;
use napi::JsObject;
use std::panic::set_hook;

#[module_exports]
fn init(mut exports: JsObject) -> napi::Result<()> {
    if cfg!(debug_assertions) {
        set_hook(Box::new(|_panic_info| {
            let backtrace = Backtrace::new();
            println!("Backtrace: {:?}", backtrace);
        }));
    }

    exports.create_named_method("runCLI", invoke_cli)?;

    Ok(())
}

#[js_function]
fn invoke_cli(ctx: CallContext) -> napi::Result<JsObject> {
    let v = ctx.env.get_undefined()?;
    v.coerce_to_object()
}
