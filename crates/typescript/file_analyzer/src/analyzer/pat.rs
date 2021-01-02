use super::{Analyzer, Ctx};
use crate::ty::TypeExt;
use crate::util::type_ext::TypeVecExt;
use crate::{
    analyzer::util::{ResultExt, VarVisitor},
    ty,
    ty::Type,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use rnode::VisitWith;
use stc_ts_ast_rnode::RAssignPat;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RKeyValuePatProp;
use stc_ts_ast_rnode::RKeyValueProp;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RParam;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RProp;
use stc_ts_ast_rnode::RPropOrSpread;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::debug::print_type;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_types::Array;
use stc_ts_utils::PatExt;
use stc_utils::TryOpt;
use swc_atoms::js_word;
use swc_common::TypeEq;
use swc_common::{Mark, Span, Spanned, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::*;

#[derive(Debug, Clone, Copy)]
pub(super) enum PatMode {
    /// Used for assignment expressions
    Assign,
    /// Used for variable declarations, function parameters and parameter of a
    /// catch clause
    Decl,
}

impl Analyzer<'_, '_> {
    pub(crate) fn mark_as_implicit(&mut self, ty: &mut Type) {
        let span = ty.span();
        let span = span.apply_mark(self.marks().implicit_type_mark);
        ty.respan(span);
    }

    pub(crate) fn is_implicitly_typed(&self, ty: &Type) -> bool {
        self.is_implicitly_typed_span(ty.span())
    }

    pub(crate) fn is_implicitly_typed_span(&self, span: Span) -> bool {
        let mut ctxt: SyntaxContext = span.ctxt;
        loop {
            let mark = ctxt.remove_mark();

            if mark == Mark::root() {
                break;
            }

            if mark == self.marks().implicit_type_mark {
                return true;
            }
        }

        false
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RParam) -> ValidationResult<ty::FnParam> {
        node.decorators.visit_with(self);
        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            ..self.ctx
        };
        node.pat.validate_with(&mut *self.with_ctx(ctx))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RPat) -> ValidationResult<ty::FnParam> {
        self.record(p);
        if !self.is_builtin {
            debug_assert_ne!(p.span(), DUMMY_SP, "A pattern should have a valid span");
        }

        // Mark pattern as optional if default value exists
        match p {
            RPat::Assign(assign_pat) => match &*assign_pat.left {
                RPat::Ident(i) => {
                    if let Some(m) = &mut self.mutations {
                        m.for_pats.entry(i.node_id).or_default().optional = Some(true);
                    }
                }
                RPat::Array(arr) => {
                    if let Some(m) = &mut self.mutations {
                        m.for_pats.entry(arr.node_id).or_default().optional = Some(true);
                    }
                }
                RPat::Object(obj) => {
                    if let Some(m) = &mut self.mutations {
                        m.for_pats.entry(obj.node_id).or_default().optional = Some(true);
                    }
                }
                _ => {}
            },
            _ => {}
        }

        if let RPat::Assign(assign_pat) = p {
            // Handle default value

            let default_value_ty = assign_pat.right.validate_with_default(self)?;

            let ty = assign_pat
                .left
                .get_ty()
                .map(|v| v.validate_with(self))
                .unwrap_or_else(|| {
                    let mut ty = default_value_ty.generalize_lit();

                    match *ty {
                        Type::Tuple(tuple) => {
                            let mut types = tuple
                                .elems
                                .into_iter()
                                .map(|element| element.ty)
                                .collect::<Vec<_>>();

                            types.dedup_type();

                            ty = box Type::Array(Array {
                                span: tuple.span,
                                elem_type: Type::union(types),
                            });
                        }
                        _ => {}
                    }

                    Ok(ty)
                })?;

            // Remove default value.
            if let Some(pat_node_id) = assign_pat.left.node_id() {
                if let Some(m) = &mut self.mutations {
                    m.for_pats.entry(pat_node_id).or_default().ty = Some(ty)
                }
            }
        }

        let ty = p
            .node_id()
            .map(|node_id| {
                self.mutations
                    .as_ref()
                    .and_then(|m| m.for_pats.get(&node_id))
                    .and_then(|v| v.ty.clone())
            })
            .flatten()
            .map(Ok)
            .or_else(|| {
                match p.get_ty().or_else(|| match p {
                    RPat::Assign(p) => p.left.get_ty(),
                    _ => None,
                }) {
                    None => None,
                    Some(ty) => Some(ty.validate_with(self)),
                }
            })
            .try_opt()?;

        match self.ctx.pat_mode {
            PatMode::Decl => {
                match p {
                    RPat::Ident(RIdent {
                        sym: js_word!("this"),
                        ..
                    }) => {
                        assert!(ty.is_some(), "parameter named `this` should have type");
                        self.scope.this = ty.clone();
                    }
                    _ => {}
                }

                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                p.visit_with(&mut visitor);

                self.scope.declaring.extend(names.clone());

                match self.declare_vars_with_ty(VarDeclKind::Let, p, ty.clone()) {
                    Ok(()) => {}
                    Err(err) => {
                        self.storage.report(err);
                    }
                }

                self.scope.remove_declaring(names);
            }

            PatMode::Assign => {}
        }

        let ty = ty.unwrap_or_else(|| {
            if self.ctx.in_argument {
                Type::unknown(p.span())
            } else {
                Type::any(p.span())
            }
        });

        if p.get_ty().is_none() {
            if let Some(node_id) = p.node_id() {
                if let Some(m) = &mut self.mutations {
                    if m.for_pats.entry(node_id).or_default().ty.is_none() {
                        m.for_pats.entry(node_id).or_default().ty = Some(ty.clone())
                    }
                }
            }
        }

        Ok(ty::FnParam {
            span: p.span(),
            pat: p.clone(),
            required: match p {
                RPat::Ident(i) => !i.optional,
                RPat::Array(arr) => !arr.optional,
                RPat::Object(obj) => !obj.optional,
                _ => true,
            },
            ty,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RRestPat) {
        p.visit_children_with(self);

        let mut errors = Errors::default();

        if let RPat::Assign(RAssignPat { ref right, .. }) = *p.arg {
            let res: Result<_, _> = try {
                let value_ty = right.validate_with_default(self)?;

                match value_ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(Error::TS2370 { span: p.dot3_token })?,
                }
            };
            res.store(&mut errors);
        } else if let Some(ref type_ann) = p.type_ann {
            let res: Result<_, _> = try {
                let ty = type_ann.validate_with(self)?;

                match *ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(Error::TS2370 { span: p.dot3_token })?,
                }
            };

            res.store(&mut errors);
        }

        self.storage.report_all(errors);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RAssignPat) {
        p.visit_children_with(self);

        //
        match *p.left {
            RPat::Object(ref left) => {
                //
                match *p.right {
                    RExpr::Object(ref right) => {
                        'l: for e in &right.props {
                            match e {
                                RPropOrSpread::Prop(ref prop) => {
                                    //
                                    for lp in &left.props {
                                        match lp {
                                            RObjectPatProp::KeyValue(RKeyValuePatProp {
                                                key: ref pk,
                                                ..
                                            }) => {
                                                //
                                                match **prop {
                                                    RProp::KeyValue(RKeyValueProp {
                                                        ref key,
                                                        ..
                                                    }) => {
                                                        if pk.type_eq(key) {
                                                            continue 'l;
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }
                                            _ => {}
                                        }
                                    }

                                    self.storage.report(Error::TS2353 { span: prop.span() })
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {
                        // TODO: Report an error
                    }
                }
            }
            _ => {}
        }

        Ok(())
    }
}
