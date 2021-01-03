use rnode::RNode;
use stc_ts_ast_rnode::RTsType;
use stc_ts_types::Type;
use std::sync::Arc;
use swc_common::errors::Handler;
use swc_common::SourceMap;
use swc_common::Span;
use swc_ecma_codegen::text_writer::JsWriter;
use swc_ecma_codegen::Emitter;
use swc_ecma_codegen::Node;

#[derive(Clone)]
pub struct Debugger {
    #[cfg(debug_assertions)]
    pub cm: Arc<SourceMap>,
    #[cfg(debug_assertions)]
    pub handler: Arc<Handler>,
}

#[cfg(debug_assertions)]
impl Debugger {
    fn dump(&self, ty: &Type) -> String {
        let ty = RTsType::from(ty.clone());
        let ty = ty.into_orig();
        let mut buf = vec![];

        {
            let mut emitter = Emitter {
                cfg: swc_ecma_codegen::Config { minify: false },
                cm: self.cm.clone(),
                comments: None,
                wr: box JsWriter::new(self.cm.clone(), "\n", &mut buf, None),
            };

            ty.emit_with(&mut emitter).unwrap();
        }

        String::from_utf8_lossy(&buf).to_string()
    }

    pub fn dump_type(&self, span: Span, ty: &Type) {
        let ty_str = self.dump(ty);
        self.handler
            .struct_span_warn(span, "Type")
            .note(&ty_str)
            .emit();
    }
}

#[cfg(not(debug_assertions))]
impl Debugger {
    pub fn dump_type(&self, _: Span, _: &Type) {}
}
