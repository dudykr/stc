use super::{Analyzer, Ctx};
use crate::errors::Errors;
use crate::util::type_ext::TypeVecExt;
use crate::{
    analyzer::util::{ResultExt, VarVisitor},
    errors::Error,
    ty,
    ty::{Type, TypeExt},
    util::{map_with_mut::MapWithMut, PatExt},
    validator,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use stc_types::{eq::TypeEq, Array};
use swc_atoms::js_word;
use swc_common::{Mark, Span, Spanned, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_visit::VisitMutWith;

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
    fn validate(&mut self, node: &mut Param) -> ValidationResult<ty::FnParam> {
        node.decorators.visit_mut_with(self);
        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            ..self.ctx
        };
        node.pat.validate_with(&mut *self.with_ctx(ctx))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &mut Pat) -> ValidationResult<ty::FnParam> {
        use swc_ecma_visit::VisitWith;

        self.record(p);
        if !self.is_builtin {
            debug_assert_ne!(p.span(), DUMMY_SP, "A pattern should have a valid span");
        }

        // Mark pattern as optional if default value exists
        match p {
            Pat::Assign(assign_pat) => match &mut *assign_pat.left {
                Pat::Ident(i) => {
                    i.optional = true;
                }
                Pat::Array(arr) => {
                    arr.optional = true;
                }
                Pat::Object(obj) => {
                    obj.optional = true;
                }
                _ => {}
            },
            _ => {}
        }

        if let Pat::Assign(assign_pat) = p {
            // Handle default value

            let default_value_ty = assign_pat.right.validate_with_default(self)?;

            let type_ann = assign_pat
                .left
                .get_mut_ty()
                .map(|v| box v.take())
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

                    ty.into()
                });

            // Remove default value.
            *p = assign_pat.left.take();
            p.set_ty(Some(type_ann));
        }

        let ty = match p.get_mut_ty() {
            None => None,
            Some(ty) => Some(ty.validate_with(self)?),
        };

        match self.ctx.pat_mode {
            PatMode::Decl => {
                match p {
                    Pat::Ident(Ident {
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

                p.visit_with(&Invalid { span: DUMMY_SP }, &mut visitor);

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
            p.set_ty(Some(box ty.clone().into()));
        }

        Ok(ty::FnParam {
            span: p.span(),
            pat: p.clone(),
            required: match p {
                Pat::Ident(i) => !i.optional,
                Pat::Array(arr) => !arr.optional,
                Pat::Object(obj) => !obj.optional,
                _ => true,
            },
            ty,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &mut RestPat) {
        p.visit_mut_children_with(self);

        let mut errors = Errors::default();

        if let Pat::Assign(AssignPat { ref mut right, .. }) = *p.arg {
            let res: Result<_, _> = try {
                let value_ty = right.validate_with_default(self)?;

                match value_ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(Error::TS2370 { span: p.dot3_token })?,
                }
            };
            res.store(&mut errors);
        } else if let Some(ref mut type_ann) = p.type_ann {
            let res: Result<_, _> = try {
                let ty = type_ann.validate_with(self)?;

                match *ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(TsKeywordType {
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
    fn validate(&mut self, p: &mut AssignPat) {
        p.visit_mut_children_with(self);

        //
        match *p.left {
            Pat::Object(ref left) => {
                //
                match *p.right {
                    Expr::Object(ref right) => {
                        'l: for e in &right.props {
                            match e {
                                PropOrSpread::Prop(ref prop) => {
                                    //
                                    for lp in &left.props {
                                        match lp {
                                            ObjectPatProp::KeyValue(KeyValuePatProp {
                                                key: ref pk,
                                                ..
                                            }) => {
                                                //
                                                match **prop {
                                                    Prop::KeyValue(KeyValueProp {
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
