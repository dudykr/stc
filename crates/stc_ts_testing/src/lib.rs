#![allow(incomplete_features)]
#![feature(box_syntax)]
#![feature(specialization)]

use rnode::IntoRNode;
use stc_ts_ast_rnode::RModule;
use swc_common::{comments::Comments, errors::HANDLER, input::SourceFileInput, Mark, SourceFile};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_parser::{lexer::Lexer, Parser, Syntax, TsConfig};
use swc_ecma_transforms::resolver;
use swc_ecma_visit::VisitMutWith;

pub mod tsc;
pub mod visualizer;

pub fn parse(fm: &SourceFile, comments: &dyn Comments, unresolved_mark: Mark, top_level_mark: Mark) -> Module {
    let lexer = Lexer::new(
        Syntax::Typescript(TsConfig {
            tsx: fm.name.to_string().ends_with(".tsx"),
            decorators: true,
            dts: fm.name.to_string().ends_with(".d.ts"),
            no_early_errors: false,
        }),
        EsVersion::latest(),
        SourceFileInput::from(&*fm),
        Some(comments),
    );
    let mut parser = Parser::new_from(lexer);
    let mut m = parser
        .parse_module()
        .map_err(|err| {
            HANDLER.with(|handler| {
                err.into_diagnostic(handler).emit();
                ()
            })
        })
        .unwrap();

    m.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, true));

    m
}

pub fn parse_rnode(fm: &SourceFile, comments: &dyn Comments, unresolved_mark: Mark, top_level_mark: Mark) -> RModule {
    let module = parse(fm, comments, unresolved_mark, top_level_mark);

    let mut generator = rnode::NodeIdGenerator::default();
    module.into_rnode(&mut generator)
}
