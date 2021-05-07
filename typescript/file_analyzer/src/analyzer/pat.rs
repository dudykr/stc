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
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RAssignPat;
use stc_ts_ast_rnode::RAssignPatProp;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RKeyValuePatProp;
use stc_ts_ast_rnode::RKeyValueProp;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RParam;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RProp;
use stc_ts_ast_rnode::RPropOrSpread;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_types::Array;
use stc_ts_types::Key;
use stc_ts_types::PropertySignature;
use stc_ts_types::Tuple;
use stc_ts_types::TupleElement;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeLit;
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

    pub(crate) fn default_type_for_pat(&mut self, pat: &RPat) -> ValidationResult<Type> {
        let implicit_type_mark = self.marks().implicit_type_mark;

        let span = pat.span();
        match pat {
            RPat::Array(arr) => {
                return Ok(Type::Tuple(Tuple {
                    span: DUMMY_SP,
                    elems: arr
                        .elems
                        .iter()
                        .map(|elem| {
                            let span = elem.span();
                            // any
                            let ty = match elem {
                                Some(v) => self.default_type_for_pat(v)?,
                                None => Type::any(span),
                            };

                            Ok(TupleElement {
                                span,
                                // TODO?
                                label: None,
                                ty: box ty,
                            })
                        })
                        .collect::<ValidationResult<_>>()?,
                }));
            }
            RPat::Rest(r) => match &*r.arg {
                RPat::Array(..) => return self.default_type_for_pat(&r.arg),
                _ => {}
            },
            RPat::Object(obj) => {
                let mut members = Vec::with_capacity(obj.props.len());

                for props in &obj.props {
                    match props {
                        RObjectPatProp::KeyValue(p) => {
                            let key = p.key.validate_with(self)?;
                            let ty = box self.default_type_for_pat(&p.value)?;

                            members.push(TypeElement::Property(PropertySignature {
                                span: DUMMY_SP,
                                accessibility: None,
                                readonly: false,
                                key,
                                optional: false,
                                params: vec![],
                                type_ann: Some(ty),
                                type_params: None,
                            }))
                        }
                        RObjectPatProp::Assign(RAssignPatProp { key, .. }) => {
                            let key = Key::Normal {
                                span: key.span,
                                sym: key.sym.clone(),
                            };
                            members.push(TypeElement::Property(PropertySignature {
                                span: DUMMY_SP,
                                accessibility: None,
                                readonly: false,
                                key,
                                optional: false,
                                params: vec![],
                                type_ann: None,
                                type_params: None,
                            }))
                        }
                        RObjectPatProp::Rest(..) => {}
                    }
                }

                return Ok(Type::TypeLit(TypeLit {
                    span: DUMMY_SP.apply_mark(implicit_type_mark),
                    members,
                    metadata: Default::default(),
                }));
            }
            RPat::Assign(pat) => return self.default_type_for_pat(&pat.left),
            _ => {}
        }

        if self.ctx.in_argument {
            Ok(Type::unknown(pat.span()))
        } else {
            Ok(Type::any(pat.span()))
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RParam) -> ValidationResult<ty::FnParam> {
        node.decorators.visit_with(self);

        self.default_any_pat(&node.pat);

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

        if self.ctx.in_declare {
            match p {
                RPat::Assign(p) => self
                    .storage
                    .report(Error::InitializerDisallowedInAmbientContext { span: p.span }),
                _ => {}
            }
        }

        if !self.ctx.in_declare && !self.ctx.in_fn_without_body {
            match p {
                RPat::Array(RArrayPat {
                    span, optional: true, ..
                })
                | RPat::Object(RObjectPat {
                    span, optional: true, ..
                }) => self
                    .storage
                    .report(Error::OptionalBindingPatternInImplSignature { span: *span }),
                _ => {}
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
            .map(|res| res.map(|ty| ty.cheap()))
            .try_opt()?;

        // Declaring names
        let mut names = vec![];

        match self.ctx.pat_mode {
            PatMode::Decl => {
                match p {
                    RPat::Ident(RBindingIdent {
                        id: RIdent {
                            sym: js_word!("this"), ..
                        },
                        ..
                    }) => {
                        assert!(ty.is_some(), "parameter named `this` should have type");
                        self.scope.this = ty.clone();
                    }
                    _ => {}
                }

                let mut visitor = VarVisitor { names: &mut names };

                p.visit_with(&mut visitor);

                self.scope.declaring.extend(names.clone());

                if !self.is_builtin {
                    match self.declare_vars_with_ty(VarDeclKind::Let, p, ty.clone(), None) {
                        Ok(()) => {}
                        Err(err) => {
                            self.storage.report(err);
                        }
                    }
                }
            }

            PatMode::Assign => {}
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

        let res = (|| -> ValidationResult<()> {
            if let RPat::Assign(assign_pat) = p {
                // Handle default value

                let default_value_ty = assign_pat.right.validate_with_default(self)?;

                let ty = assign_pat
                    .left
                    .get_ty()
                    .map(|v| v.validate_with(self))
                    .unwrap_or_else(|| {
                        let mut ty = default_value_ty.generalize_lit().foldable();

                        match ty {
                            Type::Tuple(tuple) => {
                                let mut types = tuple.elems.into_iter().map(|element| *element.ty).collect::<Vec<_>>();

                                types.dedup_type();

                                ty = Type::Array(Array {
                                    span: tuple.span,
                                    elem_type: box Type::union(types),
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

            Ok(())
        })();

        self.scope.remove_declaring(names);

        res?;

        let ty = match ty {
            Some(v) => Some(v),
            None => match p {
                RPat::Assign(p) => Some(p.right.validate_with_default(self)?.generalize_lit()),
                _ => None,
            },
        };

        let ty = match ty {
            Some(v) => v,
            _ => self.default_type_for_pat(p)?,
        };

        if p.get_ty().is_none() {
            if let Some(node_id) = p.node_id() {
                if let Some(m) = &mut self.mutations {
                    if m.for_pats.entry(node_id).or_default().ty.is_none() {
                        m.for_pats.entry(node_id).or_default().ty = Some(ty.clone())
                    }
                }
            }
        }

        match p {
            RPat::Ident(i) if i.id.sym == *"this" => {
                self.scope.this = Some(ty.clone());
            }
            _ => {}
        }

        Ok(ty::FnParam {
            span: p.span(),
            pat: p.clone(),
            required: match p {
                RPat::Ident(i) => !i.id.optional,
                RPat::Array(arr) => !arr.optional,
                RPat::Object(obj) => !obj.optional,
                RPat::Assign(..) => false,
                RPat::Rest(..) => false,
                _ => true,
            },
            ty: box ty,
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
                                            RObjectPatProp::KeyValue(RKeyValuePatProp { key: ref pk, .. }) => {
                                                //
                                                match **prop {
                                                    RProp::KeyValue(RKeyValueProp { ref key, .. }) => {
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
