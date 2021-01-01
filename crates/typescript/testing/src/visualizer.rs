use rnode::RNode;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RTsType;
use stc_ts_types::Type;
use std::sync::Arc;
use swc_common::errors::Handler;
use swc_common::SourceMap;
use swc_common::Spanned;
use swc_ecma_codegen::text_writer::JsWriter;
use swc_ecma_codegen::Emitter;
use swc_ecma_codegen::Node;

pub struct TypeVisualizer<'a> {
    pub cm: Arc<SourceMap>,
    pub handler: &'a Handler,
}

impl Visit<Type> for TypeVisualizer<'_> {
    fn visit(&mut self, value: &Type) {
        value.visit_children_with(self);
        let span = value.span();

        let ty = RTsType::from(value.clone());
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

        let ty_str = String::from_utf8_lossy(&buf);

        self.handler
            .struct_span_warn(span, "Type")
            .note(&ty_str)
            .emit();
    }
}
