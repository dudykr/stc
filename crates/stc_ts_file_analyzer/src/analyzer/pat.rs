use rnode::VisitWith;
use stc_ts_ast_rnode::{
    RArrayPat, RAssignPat, RAssignPatProp, RBindingIdent, RExpr, RIdent, RKeyValuePatProp, RKeyValueProp, RObjectPat, RObjectPatProp,
    RParam, RPat, RProp, RPropOrSpread, RRestPat,
};
use stc_ts_errors::{ErrorKind, Errors};
use stc_ts_types::{
    Array, ArrayMetadata, CommonTypeMetadata, Instance, Key, KeywordType, PropertySignature, RestType, Tuple, TupleElement, TypeElMetadata,
    TypeElement, TypeLit, TypeLitMetadata,
};
use stc_ts_utils::PatExt;
use stc_utils::{cache::Freeze, dev_span, ext::TypeVecExt};
use swc_atoms::js_word;
use swc_common::{Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;

use crate::{
    analyzer::{
        assign::AssignOpts,
        scope::{vars::DeclareVarsOpts, VarKind},
        util::{ResultExt, VarVisitor},
        Analyzer, Ctx,
    },
    ty,
    ty::{Type, TypeExt},
    util::should_instantiate_type_ann,
    validator,
    validator::ValidateWith,
    VResult,
};

#[derive(Debug, Clone, Copy)]
pub(super) enum PatMode {
    /// Used for assignment expressions
    Assign,
    /// Used for variable declarations, function parameters and parameter of a
    /// catch clause
    Decl,
}

impl Analyzer<'_, '_> {
    pub(crate) fn mark_as_implicitly_typed(&mut self, ty: &mut Type) {
        ty.metadata_mut().implicit = true;
    }

    pub(crate) fn is_implicitly_typed(&self, ty: &Type) -> bool {
        ty.metadata().implicit
    }

    pub(crate) fn default_type_for_pat(&mut self, pat: &RPat) -> VResult<Type> {
        let _tracing = dev_span!("default_type_for_pat");

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
                                None => Type::any(span, Default::default()),
                            };

                            Ok(TupleElement {
                                span,
                                // TODO?
                                label: None,
                                ty: Box::new(ty),
                                tracker: Default::default(),
                            })
                        })
                        .collect::<VResult<_>>()?,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }));
            }
            RPat::Rest(r) => {
                if let RPat::Array(..) = &*r.arg {
                    return self.default_type_for_pat(&r.arg);
                } else {
                    return Ok(Type::Rest(RestType {
                        span,
                        ty: Box::new(self.default_type_for_pat(&r.arg)?),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    }));
                }
            }
            RPat::Object(obj) => {
                let mut members = Vec::with_capacity(obj.props.len());

                for props in &obj.props {
                    match props {
                        RObjectPatProp::KeyValue(p) => {
                            let key = p.key.validate_with(self)?;
                            let ty = Box::new(self.default_type_for_pat(&p.value)?);

                            members.push(TypeElement::Property(PropertySignature {
                                span: DUMMY_SP,
                                accessibility: None,
                                readonly: false,
                                key,
                                optional: false,
                                params: vec![],
                                type_ann: Some(ty),
                                type_params: None,
                                metadata: Default::default(),
                                accessor: Default::default(),
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
                                metadata: TypeElMetadata {
                                    has_default: true,
                                    ..Default::default()
                                },
                                accessor: Default::default(),
                            }))
                        }
                        RObjectPatProp::Rest(p) => {
                            if let Some(mutations) = &mut self.mutations {
                                mutations.for_pats.entry(p.node_id).or_default().ty = Some(Type::any(p.span, Default::default()));
                            }
                        }
                    }
                }

                return Ok(Type::TypeLit(TypeLit {
                    span: DUMMY_SP,
                    members,
                    metadata: TypeLitMetadata {
                        common: CommonTypeMetadata {
                            implicit: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    tracker: Default::default(),
                }));
            }
            RPat::Assign(pat) => return self.default_type_for_pat(&pat.left),
            _ => {}
        }

        if self.ctx.in_argument {
            Ok(Type::unknown(pat.span(), Default::default()))
        } else {
            Ok(Type::any(pat.span(), Default::default()))
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RParam) -> VResult<ty::FnParam> {
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
    fn validate(&mut self, p: &RPat) -> VResult<ty::FnParam> {
        self.validate_pat(p)
    }
}

impl Analyzer<'_, '_> {
    fn validate_pat(&mut self, p: &RPat) -> VResult<ty::FnParam> {
        if !self.config.is_builtin {
            debug_assert_ne!(p.span(), DUMMY_SP, "A pattern should have a valid span");
        }

        let marks = self.marks();

        if self.ctx.in_declare {
            if let RPat::Assign(p) = p {
                self.storage
                    .report(ErrorKind::InitializerDisallowedInAmbientContext { span: p.span }.into())
            }
        }

        if !self.ctx.in_declare && !self.ctx.in_fn_without_body {
            match p {
                RPat::Array(RArrayPat { span, optional: true, .. }) | RPat::Object(RObjectPat { span, optional: true, .. }) => self
                    .storage
                    .report(ErrorKind::OptionalBindingPatternInImplSignature { span: *span }.into()),
                _ => {}
            }
        }

        let mut ty = p
            .node_id()
            .and_then(|node_id| {
                self.mutations
                    .as_ref()
                    .and_then(|m| m.for_pats.get(&node_id))
                    .and_then(|v| v.ty.clone())
            })
            .map(Ok)
            .or_else(|| {
                p.get_ty()
                    .or_else(|| match p {
                        RPat::Assign(p) => p.left.get_ty(),
                        _ => None,
                    })
                    .map(|ty| {
                        let span = ty.span();
                        ty.validate_with(self).map(|ty| {
                            if !should_instantiate_type_ann(&ty) {
                                return ty;
                            }
                            Type::Instance(Instance {
                                span,
                                ty: Box::new(ty),
                                metadata: Default::default(),
                                tracker: Default::default(),
                            })
                        })
                    })
            })
            .transpose()?
            .freezed();

        let prev_declaring_len = self.scope.borrow().declaring.len();
        // Declaring names
        let mut names = vec![];

        match self.ctx.pat_mode {
            PatMode::Decl => {
                if let RPat::Ident(RBindingIdent {
                    id: RIdent { sym: js_word!("this"), .. },
                    ..
                }) = p
                {
                    if self.ctx.set_accessor_prop || self.ctx.get_accessor_prop {
                        return Err(ErrorKind::ThisNotAllowedInAccessor { span: p.span() }.into());
                    }
                    if ty.is_some() {
                        self.scope.borrow_mut().this = ty.clone();
                    }
                }

                let mut visitor = VarVisitor { names: &mut names };

                p.visit_with(&mut visitor);

                self.scope.borrow_mut().declaring.extend(names.clone());

                if !self.config.is_builtin {
                    ty = match self.add_vars(
                        p,
                        ty.clone(),
                        None,
                        None,
                        DeclareVarsOpts {
                            kind: VarKind::Param,
                            use_iterator_for_array: false,
                        },
                    ) {
                        Ok(Some(v)) => Some(v),
                        Err(err) => {
                            self.storage.report(err);
                            None
                        }
                        Ok(None) => ty,
                    }
                }
            }

            PatMode::Assign => {}
        }

        let default_value_ty = match self.ctx.pat_mode {
            PatMode::Assign => {
                if let RPat::Assign(assign_pat) = p {
                    let ctx = Ctx {
                        array_lit_cannot_be_tuple: true,
                        ..self.ctx
                    };
                    let mut a = self.with_ctx(ctx);
                    assign_pat.right.validate_with_default(&mut *a).report(&mut a.storage)
                } else {
                    None
                }
            }
            PatMode::Decl => None,
        };

        self.scope.borrow_mut().declaring.truncate(prev_declaring_len);

        // Mark pattern as optional if default value exists
        if let RPat::Assign(assign_pat) = p {
            match &*assign_pat.left {
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
            }
        }

        let res = (|| -> VResult<()> {
            if let RPat::Assign(assign_pat) = p {
                // Handle default value
                if let Some(default_value_ty) = default_value_ty.clone() {
                    let ty = assign_pat.left.get_ty().map(|v| v.validate_with(self));

                    // If pat mode is declare, assignment of default value will be handled by
                    // variable declarator function.
                    if let PatMode::Assign = self.ctx.pat_mode {
                        if let Some(Ok(ty)) = &ty {
                            self.assign_with_opts(
                                &mut Default::default(),
                                ty,
                                &default_value_ty,
                                AssignOpts {
                                    span: assign_pat.span,
                                    ..Default::default()
                                },
                            )
                            .report(&mut self.storage);
                        }
                    }

                    let ty = ty.unwrap_or_else(|| {
                        let mut ty = default_value_ty.generalize_lit().foldable();

                        if matches!(ty.normalize(), Type::Tuple(..)) {
                            ty.normalize_mut();
                            match ty {
                                Type::Tuple(tuple) => {
                                    let mut types = tuple.elems.into_iter().map(|element| *element.ty).collect::<Vec<_>>();

                                    types.dedup_type();

                                    ty = Type::Array(Array {
                                        span: tuple.span,
                                        elem_type: Box::new(Type::new_union(tuple.span, types)),
                                        metadata: ArrayMetadata {
                                            common: tuple.metadata.common,
                                            ..Default::default()
                                        },
                                        tracker: Default::default(),
                                    });
                                }
                                _ => {
                                    unreachable!();
                                }
                            }
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
            }

            Ok(())
        })();

        res?;

        let ty = match ty {
            Some(v) => Some(v),
            None => match p {
                RPat::Assign(p) => match self.ctx.pat_mode {
                    PatMode::Decl => None,
                    PatMode::Assign => Some(default_value_ty.unwrap_or_else(|| Type::any(p.span, Default::default()))),
                },
                _ => None,
            },
        };

        let ty = match ty {
            Some(v) => v,
            _ => self.default_type_for_pat(p)?,
        }
        .freezed();

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
                self.scope.borrow_mut().this = Some(ty.clone());
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
            ty: Box::new(ty),
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
                    | Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(ErrorKind::TS2370 { span: p.dot3_token })?,
                }
            };
            res.store(&mut errors);
        } else if let Some(ref type_ann) = p.type_ann {
            let res: Result<_, _> = try {
                let ty = type_ann.validate_with(self)?;

                match *ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(KeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(ErrorKind::TS2370 { span: p.dot3_token })?,
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
        if let RPat::Object(ref left) = *p.left {
            //
            if let RExpr::Object(ref right) = *p.right {
                'l: for e in &right.props {
                    if let RPropOrSpread::Prop(ref prop) = e {
                        //
                        for lp in &left.props {
                            if let RObjectPatProp::KeyValue(RKeyValuePatProp { key: ref pk, .. }) = lp {
                                //
                                if let RProp::KeyValue(RKeyValueProp { ref key, .. }) = **prop {
                                    if pk.type_eq(key) {
                                        continue 'l;
                                    }
                                }
                            }
                        }

                        self.storage.report(ErrorKind::TS2353 { span: prop.span() }.into())
                    }
                }
            }
        }

        Ok(())
    }
}
