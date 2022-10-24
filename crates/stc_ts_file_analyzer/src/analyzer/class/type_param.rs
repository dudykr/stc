use rnode::{Visit, VisitWith};
use stc_ts_errors::Error;
use stc_ts_types::{Id, TypeParam};
use swc_common::Span;

use crate::analyzer::{Analyzer, ScopeKind};

impl Analyzer<'_, '_> {
    pub(crate) fn is_type_param_declared_in_containing_class(&mut self, id: &Id) -> bool {
        self.scope
            .first(|scope| {
                let parent = scope.parent();
                let parent = match parent {
                    Some(v) => v,
                    None => return false,
                };
                if scope.kind() != ScopeKind::Class {
                    return false;
                }
                dbg!(&scope.declaring_type_params);
                scope.declaring_type_params.contains(&id)
            })
            .is_some()
    }
}

pub(super) struct StaticTypeParamValidator<'a, 'b, 'c> {
    pub span: Span,
    pub analyzer: &'a mut Analyzer<'b, 'c>,
}

impl Visit<TypeParam> for StaticTypeParamValidator<'_, '_, '_> {
    fn visit(&mut self, param: &TypeParam) {
        param.visit_children_with(self);

        if self
            .analyzer
            .is_type_param_declared_in_containing_class(&param.name)
        {
            self.analyzer
                .storage
                .report(Error::StaticMemberCannotUseTypeParamOfClass { span: self.span })
        }
    }
}
