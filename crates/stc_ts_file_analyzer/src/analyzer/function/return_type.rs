use rnode::{VisitMut, VisitMutWith};
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_types::{QueryExpr, QueryType, Type};

use crate::{
    analyzer::{scope::VarInfo, Analyzer},
    VResult,
};

impl Analyzer<'_, '_> {
    pub(crate) fn expand_return_type_of_fn(&mut self, ret_ty: &mut Type) -> VResult<()> {
        if self.config.is_builtin {
            return Ok(());
        }

        ret_ty.visit_mut_with(&mut FnReturnTypeHandler { analyzer: self });
        Ok(())
    }
}

struct FnReturnTypeHandler<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}

impl VisitMut<Type> for FnReturnTypeHandler<'_, '_, '_> {
    fn visit_mut(&mut self, ret_ty: &mut Type) {
        ret_ty.visit_mut_children_with(self);

        if ret_ty.is_query() {
            if let Type::Query(QueryType {
                expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(var_name)),
                ..
            }) = ret_ty
            {
                // We only check for typeof for variables declared in current scope.
                //
                // This is effectively checking for parameters, because the return type expanded
                // runs after validation parameters and before validating bodies.
                if let Some(VarInfo {
                    copied: false,
                    ty: Some(ty),
                    ..
                }) = self.analyzer.scope.vars.get(&var_name.clone().into())
                {
                    *ret_ty = ty.clone().into_type();
                }
            }
        }
    }
}
