use crate::analyzer::Analyzer;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use stc_ts_ast_rnode::RAwaitExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::Ref;
use stc_ts_types::Type;
use swc_atoms::js_word;

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RAwaitExpr) -> ValidationResult {
        let arg_ty = e.arg.validate_with_default(self)?;

        // TODO: Check if the `Promise` is that of global.
        match &*arg_ty {
            Type::Ref(Ref {
                type_name: RTsEntityName::Ident(i),
                type_args: Some(type_args),
                ..
            }) => {
                if i.sym == js_word!("Promise") {
                    if !type_args.params.is_empty() {
                        return Ok(type_args.params[0].clone());
                    }
                }
            }
            _ => {}
        }

        Ok(arg_ty)
    }
}
