#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(specialization)]

use swc_common::{comments::Comments, input::SourceFileInput, sync::Lrc, SourceFile};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_utils::HANDLER;

pub mod tsc;
pub mod visualizer;

pub fn parse(fm: Lrc<SourceFile>, comments: &dyn Comments) -> Module {
    let lexer = Lexer::new(
        Syntax::Typescript(TsConfig {
            tsx: fm.name.to_string().ends_with(".tsx"),
            decorators: true,
            dynamic_import: true,
            dts: fm.name.to_string().ends_with(".d.ts"),
            no_early_errors: false,
            import_assertions: true,
        }),
        EsVersion::latest(),
        SourceFileInput::from(&*fm),
        Some(comments),
    );
    let mut parser = Parser::new_from(lexer);
    parser
        .parse_module()
        .map_err(|err| {
            HANDLER.with(|handler| {
                err.into_diagnostic(handler).emit();
                ()
            })
        })
        .unwrap()
}
