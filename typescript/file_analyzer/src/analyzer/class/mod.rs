use self::type_param::StaticTypeParamValidator;
use super::assign::AssignOpts;
use super::expr::TypeOfMode;
use super::props::ComputedPropMode;
use super::util::instantiate_class;
use super::util::is_prop_name_eq;
use super::util::ResultExt;
use super::util::VarVisitor;
use super::Analyzer;
use super::Ctx;
use super::ScopeKind;
use crate::env::ModuleConfig;
use crate::ty::LitGeneralizer;
use crate::ty::TypeExt;
use crate::util::property_map::PropertyMap;
use crate::validator;
use crate::validator::ValidateWith;
use crate::ValidationResult;
use itertools::Itertools;
use rnode::FoldWith;
use rnode::IntoRNode;
use rnode::NodeId;
use rnode::NodeIdGenerator;
use rnode::VisitWith;
use stc_ts_ast_rnode::RAssignPat;
use stc_ts_ast_rnode::RBindingIdent;
use stc_ts_ast_rnode::RClass;
use stc_ts_ast_rnode::RClassDecl;
use stc_ts_ast_rnode::RClassExpr;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RClassMethod;
use stc_ts_ast_rnode::RClassProp;
use stc_ts_ast_rnode::RConstructor;
use stc_ts_ast_rnode::RDecl;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RParam;
use stc_ts_ast_rnode::RParamOrTsParamProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPrivateMethod;
use stc_ts_ast_rnode::RPrivateProp;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsFnParam;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsParamProp;
use stc_ts_ast_rnode::RTsParamPropParam;
use stc_ts_ast_rnode::RTsTypeAliasDecl;
use stc_ts_ast_rnode::RTsTypeAnn;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_errors::Errors;
use stc_ts_file_analyzer_macros::extra_validator;
use stc_ts_types::Class;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassMember;
use stc_ts_types::ClassProperty;
use stc_ts_types::ComputedKey;
use stc_ts_types::ConstructorSignature;
use stc_ts_types::FnParam;
use stc_ts_types::Id;
use stc_ts_types::Intersection;
use stc_ts_types::Key;
use stc_ts_types::Method;
use stc_ts_types::Operator;
use stc_ts_types::QueryExpr;
use stc_ts_types::QueryType;
use stc_ts_types::Ref;
use stc_ts_types::TsExpr;
use stc_ts_types::Type;
use stc_ts_utils::PatExt;
use stc_utils::TryOpt;
use std::mem::replace;
use std::mem::take;
use swc_atoms::js_word;
use swc_common::EqIgnoreSpan;
use swc_common::Span;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::private_ident;

mod order;
mod type_param;

#[derive(Debug, Default)]
pub(crate) struct ClassState {}

impl Analyzer<'_, '_> {
    fn validate_type_of_class_property(
        &mut self,
        span: Span,
        readonly: bool,
        is_static: bool,
        type_ann: &Option<RTsTypeAnn>,
        value: &Option<Box<RExpr>>,
    ) -> ValidationResult<Option<Type>> {
        let mut ty = try_opt!(type_ann.validate_with(self));
        let mut value_ty = {
            let ctx = Ctx {
                in_static_property_initializer: is_static,
                ..self.ctx
            };
            try_opt!(value.validate_with_args(&mut *self.with_ctx(ctx), (TypeOfMode::RValue, None, ty.as_ref())))
        };

        if readonly {
            if let Some(ty) = &mut ty {
                self.prevent_generalize(ty);
            }
            if let Some(ty) = &mut value_ty {
                self.prevent_generalize(ty);
            }
        }

        Ok(ty.or_else(|| value_ty).map(|ty| match ty {
            Type::Symbol(..) if readonly && is_static => Type::Operator(Operator {
                span: ty.span(),
                op: TsTypeOperatorOp::Unique,
                ty: box Type::Keyword(RTsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                }),
            }),
            _ => ty,
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RClassProp) -> ValidationResult<ClassProperty> {
        self.record(p);

        if !p.computed && p.is_static {
            match &*p.key {
                RExpr::Ident(i) => {
                    if &*i.sym == "prototype" {
                        self.storage
                            .report(Error::StaticPropertyCannotBeNamedProptotype { span: i.span })
                    }
                }
                _ => {}
            }
        }

        // Verify key if key is computed
        if p.computed {
            self.validate_computed_prop_key(p.span, &p.key);
        }

        let value = self
            .validate_type_of_class_property(p.span, p.readonly, p.is_static, &p.type_ann, &p.value)?
            .map(Box::new);

        if p.is_static {
            value.visit_with(&mut StaticTypeParamValidator {
                span: p.span,
                analyzer: self,
            });
        }

        match p.accessibility {
            Some(Accessibility::Private) => {}
            _ => {
                if p.type_ann.is_none() {
                    if let Some(m) = &mut self.mutations {
                        m.for_class_props.entry(p.node_id).or_default().ty =
                            value.clone().map(|ty| ty.generalize_lit());
                    }
                }
            }
        }

        let key = self.validate_key(&p.key, p.computed)?;

        Ok(ClassProperty {
            span: p.span,
            key,
            value,
            is_static: p.is_static,
            accessibility: p.accessibility,
            is_abstract: p.is_abstract,
            is_optional: p.is_optional,
            readonly: p.readonly,
            definite: p.definite,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RPrivateProp) -> ValidationResult<ClassProperty> {
        match p.key.id.sym {
            js_word!("constructor") => {
                self.storage.report(Error::ConstructorIsKeyword { span: p.key.id.span });
            }
            _ => {}
        }

        let key = Key::Private(p.key.clone().into());

        let value = self
            .validate_type_of_class_property(p.span, p.readonly, p.is_static, &p.type_ann, &p.value)?
            .map(Box::new);

        if !self.ctx.in_declare && self.rule().no_implicit_any {
            if value.is_none() {
                self.storage.report(Error::ImplicitAny { span: key.span() })
            }
        }

        Ok(ClassProperty {
            span: p.span,
            key,
            value,
            is_static: p.is_static,
            accessibility: p.accessibility,
            is_abstract: p.is_abstract,
            is_optional: p.is_optional,
            readonly: p.readonly,
            definite: p.definite,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, c: &RConstructor) -> ValidationResult<ConstructorSignature> {
        self.record(c);

        let c_span = c.span();

        self.with_child(ScopeKind::Constructor, Default::default(), |child: &mut Analyzer| {
            child.ctx.in_declare |= c.body.is_none();

            let RConstructor { params, body, .. } = c;

            {
                // Validate params
                // TODO: Move this to parser
                let mut has_optional = false;
                for p in params.iter() {
                    if has_optional {
                        match p {
                            RParamOrTsParamProp::Param(RParam { pat, .. }) => match pat {
                                RPat::Ident(RBindingIdent {
                                    id: RIdent { optional: true, .. },
                                    ..
                                })
                                | RPat::Rest(..) => {}
                                _ => {
                                    child.storage.report(Error::TS1016 { span: p.span() });
                                }
                            },
                            _ => {}
                        }
                    }

                    match *p {
                        RParamOrTsParamProp::Param(RParam {
                            pat:
                                RPat::Ident(RBindingIdent {
                                    id: RIdent { optional, .. },
                                    ..
                                }),
                            ..
                        }) => {
                            if optional {
                                has_optional = true;
                            }
                        }
                        _ => {}
                    }
                }
            }

            let mut ps = Vec::with_capacity(params.len());
            for param in params.iter() {
                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                param.visit_with(&mut visitor);

                child.scope.declaring.extend(names.clone());

                let p: FnParam = param.validate_with(child)?;

                ps.push(p);

                child.scope.remove_declaring(names);
            }

            c.body.visit_with(child);

            Ok(ConstructorSignature {
                accessibility: c.accessibility,
                span: c.span,
                params: ps,
                ret_ty: None,
                type_params: None,
            })
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RTsParamProp) -> ValidationResult<FnParam> {
        if self.ctx.in_declare {
            match p.param {
                RTsParamPropParam::Assign(..) => self
                    .storage
                    .report(Error::InitializerDisallowedInAmbientContext { span: p.span }),
                _ => {}
            }
        }

        match &p.param {
            RTsParamPropParam::Ident(ref i)
            | RTsParamPropParam::Assign(RAssignPat {
                left: box RPat::Ident(ref i),
                ..
            }) => {
                let ty: Option<Type> = i.type_ann.validate_with(self).try_opt()?;
                let ty = ty.map(|ty| ty.cheap());

                self.declare_var(
                    i.id.span,
                    VarDeclKind::Let,
                    i.id.clone().into(),
                    ty.clone(),
                    None,
                    true,
                    false,
                    false,
                )?;

                Ok(FnParam {
                    span: p.span,
                    required: !i.id.optional,
                    pat: RPat::Ident(i.clone()),
                    ty: box ty.unwrap_or_else(|| Type::any(i.id.span)),
                })
            }
            _ => unreachable!(),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, p: &RTsFnParam) -> ValidationResult<FnParam> {
        self.record(p);

        let span = p.span();

        macro_rules! ty {
            ($node_id:expr, $e:expr) => {{
                match self
                    .mutations
                    .as_mut()
                    .map(|m| m.for_pats.get(&$node_id).map(|p| p.ty.clone()))
                    .flatten()
                    .flatten()
                {
                    Some(ty) => box ty,
                    None => {
                        let e: Option<_> = $e.validate_with(self).try_opt()?;
                        box e.unwrap_or_else(|| {
                            let mut ty = Type::any(span);
                            self.mark_as_implicit(&mut ty);
                            ty
                        })
                    }
                }
            }};
        }

        Ok(match p {
            RTsFnParam::Ident(i) => FnParam {
                span,
                pat: RPat::Ident(i.clone()),
                required: !i.id.optional,
                ty: ty!(i.node_id, i.type_ann),
            },
            RTsFnParam::Array(p) => FnParam {
                span,
                pat: RPat::Array(p.clone()),
                required: true,
                ty: ty!(p.node_id, p.type_ann),
            },
            RTsFnParam::Rest(p) => FnParam {
                span,
                pat: RPat::Rest(p.clone()),
                required: false,
                ty: ty!(p.node_id, p.type_ann),
            },
            RTsFnParam::Object(p) => FnParam {
                span,
                pat: RPat::Object(p.clone()),
                required: true,
                ty: ty!(p.node_id, p.type_ann),
            },
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, c: &RPrivateMethod) -> ValidationResult<Method> {
        match c.key.id.sym {
            js_word!("constructor") => {
                self.storage.report(Error::ConstructorIsKeyword { span: c.key.id.span });
            }
            _ => {}
        }

        match c.kind {
            MethodKind::Method => {
                self.storage
                    .report(Error::PrivateIdUsedAsMethodName { span: c.key.id.span });
            }
            _ => {}
        }

        let key = c.key.validate_with(self).map(Key::Private)?;
        let key_span = key.span();

        let (type_params, params, ret_ty) = self.with_child(
            ScopeKind::Method { is_static: c.is_static },
            Default::default(),
            |child: &mut Analyzer| -> ValidationResult<_> {
                let type_params = try_opt!(c.function.type_params.validate_with(child));
                if (c.kind == MethodKind::Getter || c.kind == MethodKind::Setter) && type_params.is_some() {
                    child.storage.report(Error::TS1094 { span: key_span })
                }

                let params = c.function.params.validate_with(child)?;

                let declared_ret_ty = try_opt!(c.function.return_type.validate_with(child));

                let span = c.function.span;
                let is_async = c.function.is_async;
                let is_generator = c.function.is_generator;

                let inferred_ret_ty = match c
                    .function
                    .body
                    .as_ref()
                    .map(|bs| child.visit_stmts_for_return(span, is_async, is_generator, &bs.stmts))
                {
                    Some(Ok(ty)) => ty,
                    Some(err) => err?,
                    None => None,
                };

                Ok((
                    type_params,
                    params,
                    box declared_ret_ty
                        .or_else(|| inferred_ret_ty)
                        .unwrap_or_else(|| Type::any(key_span)),
                ))
            },
        )?;

        Ok(Method {
            span: c.span,
            key,
            type_params,
            params,
            ret_ty,
            kind: c.kind,
            accessibility: None,
            is_static: c.is_static,
            is_abstract: c.is_abstract,
            is_optional: c.is_optional,
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, c: &RClassMethod) -> ValidationResult<ClassMember> {
        self.record(c);

        let key = c.key.validate_with(self)?;

        let c_span = c.span();
        let key_span = c.key.span();

        let (params, type_params, declared_ret_ty, inferred_ret_ty) = self.with_child(
            ScopeKind::Method { is_static: c.is_static },
            Default::default(),
            |child: &mut Analyzer| -> ValidationResult<_> {
                child.ctx.in_declare |= c.function.body.is_none();
                child.ctx.in_async |= c.function.is_async;
                child.ctx.in_generator |= c.function.is_generator;

                child.scope.declaring_prop = match &key {
                    Key::Normal { sym, .. } => Some(Id::word(sym.clone())),
                    _ => None,
                };

                {
                    // It's error if abstract method has a body

                    if c.is_abstract && c.function.body.is_some() {
                        child.storage.report(Error::TS1318 { span: key_span });
                    }
                }

                {
                    // Validate params
                    // TODO: Move this to parser
                    let mut has_optional = false;
                    for p in &c.function.params {
                        if has_optional {
                            match p.pat {
                                RPat::Ident(RBindingIdent {
                                    id: RIdent { optional: true, .. },
                                    ..
                                })
                                | RPat::Rest(..) => {}
                                _ => {
                                    child.storage.report(Error::TS1016 { span: p.span() });
                                }
                            }
                        }

                        match p.pat {
                            RPat::Ident(RBindingIdent {
                                id: RIdent { optional, .. },
                                ..
                            }) => {
                                if optional {
                                    has_optional = true;
                                }
                            }
                            _ => {}
                        }
                    }
                }

                let type_params = try_opt!(c.function.type_params.validate_with(child));
                if (c.kind == MethodKind::Getter || c.kind == MethodKind::Setter) && type_params.is_some() {
                    child.storage.report(Error::TS1094 { span: key_span })
                }

                let params = c.function.params.validate_with(child)?;

                // c.function.visit_children_with(child);

                // if child.ctx.in_declare && c.function.body.is_some() {
                //     child.storage.report(Error::TS1183 { span: key_span })
                // }

                if c.kind == MethodKind::Setter && c.function.return_type.is_some() {
                    child.storage.report(Error::TS1095 { span: key_span })
                }

                let declared_ret_ty = try_opt!(c.function.return_type.validate_with(child));
                let declared_ret_ty = declared_ret_ty.map(|ty| ty.cheap());
                child.scope.declared_return_type = declared_ret_ty.clone();

                let span = c.function.span;
                let is_async = c.function.is_async;
                let is_generator = c.function.is_generator;

                let inferred_ret_ty = match c
                    .function
                    .body
                    .as_ref()
                    .map(|bs| child.visit_stmts_for_return(span, is_async, is_generator, &bs.stmts))
                {
                    Some(Ok(ty)) => ty,
                    Some(err) => err?,
                    None => None,
                };

                Ok((params, type_params, declared_ret_ty, inferred_ret_ty))
            },
        )?;

        if c.kind == MethodKind::Getter && c.function.body.is_some() {
            // Inferred return type.

            // getter property must have return statements.
            if let None = inferred_ret_ty {
                self.storage.report(Error::TS2378 { span: key_span });
            }
        }

        let ret_ty = box declared_ret_ty.unwrap_or_else(|| {
            inferred_ret_ty.map(|ty| ty.generalize_lit()).unwrap_or_else(|| {
                Type::Keyword(RTsKeywordType {
                    span: c_span,
                    kind: if c.function.body.is_some() {
                        TsKeywordTypeKind::TsVoidKeyword
                    } else {
                        TsKeywordTypeKind::TsAnyKeyword
                    },
                })
            })
        });

        if c.kind != MethodKind::Setter {
            let node_id = c.function.node_id;

            let ret_ty = if self.may_generalize(&ret_ty) {
                ret_ty.clone().generalize_lit()
            } else {
                *ret_ty.clone()
            };
            if let Some(m) = &mut self.mutations {
                m.for_fns.entry(node_id).or_default().ret_ty = Some(ret_ty);
            }
        }

        Ok(ClassMember::Method(Method {
            span: c_span,
            key,
            accessibility: c.accessibility,
            is_static: c.is_static,
            is_abstract: c.is_abstract,
            is_optional: c.is_optional,
            type_params,
            params,
            ret_ty,
            kind: c.kind,
        }))
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, m: &RClassMember) -> ValidationResult<Option<ClassMember>> {
        Ok(match m {
            RClassMember::PrivateMethod(m) => Some(m.validate_with(self).map(From::from)?),
            RClassMember::PrivateProp(m) => Some(m.validate_with(self).map(From::from)?),
            RClassMember::Empty(..) => None,

            RClassMember::Constructor(v) => Some(ClassMember::Constructor(v.validate_with(self)?)),
            RClassMember::Method(method) => {
                let v = method.validate_with(self)?;

                if let Some(Accessibility::Private) = method.accessibility {
                    let computed = match method.key {
                        RPropName::Computed(_) => true,
                        _ => false,
                    };
                }

                Some(v)
            }
            RClassMember::ClassProp(v) => Some(ClassMember::Property(v.validate_with(self)?)),
            RClassMember::TsIndexSignature(v) => Some(ClassMember::IndexSignature(v.validate_with(self)?)),
        })
    }
}

impl Analyzer<'_, '_> {
    fn check_ambient_methods(&mut self, c: &RClass, declare: bool) -> ValidationResult<()> {
        if self.ctx.in_declare {
            return Ok(());
        }

        fn is_key_optional(key: &RPropName) -> bool {
            match key {
                RPropName::Ident(i) => i.optional,
                _ => false,
            }
        }

        fn is_prop_name_eq_include_computed(l: &RPropName, r: &RPropName) -> bool {
            match l {
                RPropName::Computed(l) => match r {
                    RPropName::Computed(r) => {
                        if l.eq_ignore_span(&r) {
                            // TODO: Return true only if l and r are both
                            // symbol type
                            return true;
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            is_prop_name_eq(l, r)
        }

        // Report errors for code like
        //
        //      class C {
        //           foo();
        //      }
        if declare {
            return Ok(());
        }

        let mut errors = Errors::default();
        // Span of name
        let mut spans = vec![];
        let mut name: Option<&RPropName> = None;

        for (idx, m) in c.body.iter().enumerate() {
            macro_rules! check {
                ($m:expr, $body:expr, $is_constructor:expr) => {{
                    let m = $m;

                    let computed = match m.key {
                        RPropName::Computed(..) => true,
                        _ => false,
                    };

                    if $body.is_none() {
                        if name.is_some()
                            && !is_key_optional(&m.key)
                            && !is_prop_name_eq_include_computed(&name.unwrap(), &m.key)
                        {
                            for (span, is_constructor) in replace(&mut spans, vec![]) {
                                if is_constructor {
                                    errors.push(Error::ConstructorImplMissingOrNotFollowedByDecl { span });
                                } else {
                                    errors.push(Error::FnImplMissingOrNotFollowedByDecl { span });
                                }
                            }
                        }

                        // Body of optional method can be omitted
                        if is_key_optional(&m.key) {
                            name = None;
                            spans.clear();
                        } else {
                            if name.is_some() && is_prop_name_eq_include_computed(&name.unwrap(), &m.key) {
                                spans.clear();
                            }
                            spans.push((m.key.span(), $is_constructor));

                            name = Some(&m.key);
                        }
                    } else {
                        if name.is_none() || is_prop_name_eq_include_computed(&name.unwrap(), &m.key) {
                            // TODO: Verify parameters

                            if name.is_some() {
                                if let Some(mutations) = &mut self.mutations {
                                    mutations.for_class_members.entry(m.node_id).or_default().remove = true;
                                }
                            }

                            spans.clear();
                            name = None;
                        } else {
                            let constructor_name = RPropName::Ident(RIdent::new(js_word!("constructor"), DUMMY_SP));

                            if is_prop_name_eq_include_computed(&name.unwrap(), &constructor_name) {
                                for (span, is_constructor) in replace(&mut spans, vec![]) {
                                    if is_constructor {
                                        errors.push(Error::ConstructorImplMissingOrNotFollowedByDecl { span });
                                    } else {
                                        errors.push(Error::FnImplMissingOrNotFollowedByDecl { span });
                                    }
                                }
                            } else if is_prop_name_eq_include_computed(&m.key, &constructor_name) {
                                for (span, is_constructor) in replace(&mut spans, vec![]) {
                                    if is_constructor {
                                        errors.push(Error::ConstructorImplMissingOrNotFollowedByDecl { span });
                                    } else {
                                        errors.push(Error::FnImplMissingOrNotFollowedByDecl { span });
                                    }
                                }
                            } else {
                                spans = vec![];

                                errors.push(Error::TS2389 { span: m.key.span() });
                            }

                            name = None;
                        }

                        if is_key_optional(&m.key) {
                            name = None;
                            spans.clear();
                        }
                    }
                }};
            }

            match *m {
                RClassMember::Constructor(ref m) => check!(m, m.body, true),
                RClassMember::Method(ref m @ RClassMethod { is_abstract: false, .. }) => {
                    check!(m, m.function.body, false)
                }
                _ => {}
            }
        }

        // Class definition ended with `foo();`
        for (span, is_constructor) in replace(&mut spans, vec![]) {
            if is_constructor {
                errors.push(Error::ConstructorImplMissingOrNotFollowedByDecl { span });
            } else {
                errors.push(Error::FnImplMissingOrNotFollowedByDecl { span });
            }
        }

        self.storage.report_all(errors);

        Ok(())
    }

    #[extra_validator]
    pub(super) fn validate_computed_prop_key(&mut self, span: Span, key: &RExpr) {
        if self.is_builtin {
            // We don't need to validate builtins
            return;
        }

        let mut errors = Errors::default();
        let is_symbol_access = match *key {
            RExpr::Member(RMemberExpr {
                obj:
                    RExprOrSuper::Expr(box RExpr::Ident(RIdent {
                        sym: js_word!("Symbol"),
                        ..
                    })),
                ..
            }) => true,
            _ => false,
        };

        let ty = match key.validate_with_default(self).map(|mut ty| {
            ty.respan(span);
            ty
        }) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                    _ => {}
                }

                errors.push(err);

                Type::any(span)
            }
        };

        match *ty.normalize() {
            Type::Lit(..) => {}
            Type::Operator(Operator {
                op: TsTypeOperatorOp::Unique,
                ty:
                    box Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    }),
                ..
            }) => {}
            _ if is_symbol_access => {}
            _ => errors.push(Error::TS1166 { span }),
        }

        if !errors.is_empty() {
            Err(Error::Errors {
                span,
                errors: errors.into(),
            })?
        }
    }

    fn validate_interface_conflicts(&mut self, interfaces: &[TsExpr]) {
        if self.is_builtin {
            return;
        }
    }

    fn validate_inherited_members_from_interfaces(&mut self, name: Option<Span>, class: &ClassDef) {
        if self.is_builtin {
            return;
        }

        if class.is_abstract || self.ctx.in_declare {
            return;
        }

        let class_ty = Type::Class(Class {
            span: class.span,
            def: box class.clone(),
        });

        for parent in &*class.implements {
            let res: ValidationResult<_> = try {
                let parent = self.type_of_ts_entity_name(
                    parent.span(),
                    self.ctx.module_id,
                    &parent.expr,
                    parent.type_args.as_deref(),
                )?;

                self.assign_with_opts(
                    &mut Default::default(),
                    AssignOpts {
                        span: parent.span(),
                        allow_unknown_rhs: true,
                        ..Default::default()
                    },
                    &parent,
                    &class_ty,
                )
                .context("tried to assign a class to parent interface")
                .convert_err(|err| {
                    let span = err.span();
                    if err.code() == 2322 {
                        Error::Errors {
                            span,
                            errors: err
                                .into_causes()
                                .into_iter()
                                .map(|err| {
                                    err.convert(|err| Error::InvalidImplOfInterface {
                                        span: match &err {
                                            Error::AssignFailed {
                                                right_ident: Some(s), ..
                                            } => *s,
                                            Error::AssignFailed { right, .. } => right.span(),
                                            _ => err.span(),
                                        },
                                        cause: box err,
                                    })
                                })
                                .collect(),
                        }
                    } else {
                        err.convert_all(|err| {
                            match err {
                                Error::MissingFields { .. } => {
                                    return Error::ClassIncorrectlyImplementsInterface { span: parent.span() }
                                }
                                _ => {}
                            }
                            err
                        })
                    }
                })?;
            };

            res.report(&mut self.storage);
        }
    }

    /// Should be called only from `Validate<Class>`.
    fn validate_inherited_members_from_super_class(&mut self, name: Option<Span>, class: &ClassDef) {
        if class.is_abstract || self.ctx.in_declare {
            return;
        }

        let name_span = name.unwrap_or_else(|| {
            // TODD: c.span().lo() + BytePos(5) (aka class token)
            class.span
        });
        let mut errors = Errors::default();

        let res: ValidationResult<()> = try {
            if let Some(ref super_ty) = class.super_class {
                self.validate_super_class(super_ty);

                match super_ty.normalize() {
                    Type::ClassDef(sc) => {
                        'outer: for sm in &sc.body {
                            match sm {
                                ClassMember::Method(sm) => {
                                    if sm.is_optional || !sm.is_abstract {
                                        // TODO: Validate parameters

                                        // TODO: Validate return type
                                        continue 'outer;
                                    }

                                    for m in &class.body {
                                        match m {
                                            ClassMember::Method(ref m) => {
                                                if !&m.key.type_eq(&sm.key) {
                                                    continue;
                                                }

                                                // TODO: Validate parameters

                                                // TODO: Validate return type
                                                continue 'outer;
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                _ => {
                                    // TODO: Verify
                                    continue 'outer;
                                }
                            }

                            errors.push(Error::TS2515 { span: name_span });

                            if sc.is_abstract {
                                // TODO: Check super class of super class
                            }
                        }
                    }
                    _ => {}
                }
            }
        };

        if let Err(err) = res {
            errors.push(err);
        }

        self.storage.report_all(errors);
    }
}

/// Order:
///
/// 1. static properties
/// 2. static methods, using dependency graph.
/// 3. TsParamProp in constructors.
/// 4. Properties from top to bottom.
/// 5. Others, using dependency graph.
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, c: &RClass) -> ValidationResult<ClassDef> {
        self.record(c);

        self.ctx.computed_prop_mode = ComputedPropMode::Class {
            has_body: !self.ctx.in_declare,
        };

        c.decorators.visit_with(self);
        let name = self.scope.this_class_name.take();
        match &name {
            Some(i) => match &**i.sym() {
                "any" | "void" | "never" | "string" | "number" | "boolean" | "null" | "undefined" | "symbol" => {
                    self.storage.report(Error::InvalidClassName { span: c.span });
                }
                "Object" if self.env.target() <= EsVersion::Es5 => match self.env.module() {
                    ModuleConfig::None if self.ctx.in_declare => {}

                    ModuleConfig::None
                    | ModuleConfig::Umd
                    | ModuleConfig::System
                    | ModuleConfig::Amd
                    | ModuleConfig::CommonJs => {
                        self.storage
                            .report(Error::ClassNameCannotBeObjectWhenTargetingEs5WithModule { span: c.span });
                    }
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }

        let mut types_to_register: Vec<(Id, _)> = vec![];
        let mut additional_members = vec![];

        // Scope is required because of type parameters.
        let c = self.with_child(
            ScopeKind::Class,
            Default::default(),
            |child: &mut Analyzer| -> ValidationResult<_> {
                child.scope.declaring_type_params.extend(
                    c.type_params
                        .iter()
                        .flat_map(|decl| &decl.params)
                        .map(|param| param.name.clone().into()),
                );

                // Register the class.
                child.scope.this_class_name = name.clone();

                // We handle type parameters first.
                let type_params = try_opt!(c.type_params.validate_with(child));
                child.resolve_parent_interfaces(&c.implements);

                let super_class = {
                    // Then, we can expand super class

                    let super_type_params = try_opt!(c.super_type_params.validate_with(child));
                    match &c.super_class {
                        Some(box expr) => {
                            let need_base_class = match expr {
                                RExpr::Ident(..) => false,
                                _ => true,
                            };
                            let super_ty =
                                expr.validate_with_args(child, (TypeOfMode::RValue, super_type_params.as_ref(), None))?;

                            child.validate_with(|a| match super_ty.normalize() {
                                Type::Lit(..)
                                | Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsStringKeyword,
                                    ..
                                })
                                | Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                    ..
                                })
                                | Type::Keyword(RTsKeywordType {
                                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                                    ..
                                }) => Err(Error::InvalidSuperClass { span: super_ty.span() }),
                                _ => Ok(()),
                            });

                            match super_ty.normalize() {
                                // We should handle mixin
                                Type::Intersection(i) if need_base_class => {
                                    let mut has_class_in_super = false;
                                    let class_name = name.clone().unwrap_or_else(|| Id::word("__class".into()));
                                    let new_ty: RIdent = private_ident!(format!("{}_base", class_name.as_str()))
                                        .into_rnode(&mut NodeIdGenerator::invalid());

                                    // We should add it at same level as class
                                    types_to_register.push((new_ty.clone().into(), super_ty.clone()));

                                    let super_ty = box Type::Intersection(Intersection {
                                        types: i
                                            .types
                                            .iter()
                                            .map(|ty| {
                                                match ty.normalize() {
                                                    Type::Class(c) => {
                                                        has_class_in_super = true;
                                                        // class A -> typeof A
                                                        return c
                                                            .def
                                                            .name
                                                            .as_ref()
                                                            .map(|id| {
                                                                Type::Query(QueryType {
                                                                    span: c.span,
                                                                    expr: box QueryExpr::TsEntityName(
                                                                        id.clone().into(),
                                                                    ),
                                                                })
                                                            })
                                                            .expect("Super class should be named");
                                                    }
                                                    _ => {}
                                                }

                                                ty.clone()
                                            })
                                            .collect(),
                                        ..i.clone()
                                    });

                                    if has_class_in_super {
                                        child.prepend_stmts.push(RStmt::Decl(RDecl::Var(RVarDecl {
                                            node_id: NodeId::invalid(),
                                            span: DUMMY_SP,
                                            kind: VarDeclKind::Const,
                                            declare: false,
                                            decls: vec![RVarDeclarator {
                                                node_id: NodeId::invalid(),
                                                span: i.span,
                                                name: RPat::Ident(RBindingIdent {
                                                    node_id: NodeId::invalid(),
                                                    type_ann: Some(RTsTypeAnn {
                                                        node_id: NodeId::invalid(),
                                                        span: DUMMY_SP,
                                                        type_ann: box super_ty.into(),
                                                    }),
                                                    id: new_ty.clone(),
                                                }),
                                                init: None,
                                                definite: false,
                                            }],
                                        })));
                                    } else {
                                        child
                                            .prepend_stmts
                                            .push(RStmt::Decl(RDecl::TsTypeAlias(RTsTypeAliasDecl {
                                                node_id: NodeId::invalid(),
                                                span: DUMMY_SP,
                                                declare: false,
                                                id: new_ty.clone(),
                                                // TODO: Handle type parameters
                                                type_params: None,
                                                type_ann: box super_ty.into(),
                                            })));
                                    }

                                    if let Some(m) = &mut child.mutations {
                                        let node_id = c.node_id;
                                        m.for_classes.entry(node_id).or_default().super_class =
                                            Some(box RExpr::Ident(new_ty.clone()));
                                    }
                                    Some(box Type::Ref(Ref {
                                        span: DUMMY_SP,
                                        ctxt: child.ctx.module_id,
                                        type_name: RTsEntityName::Ident(new_ty),
                                        // TODO: Handle type parameters
                                        type_args: None,
                                    }))
                                }
                                _ => Some(box super_ty),
                            }
                        }

                        _ => None,
                    }
                };

                let implements = c.implements.validate_with(child).map(Box::new)?;

                // TODO: Check for implements

                child.check_ambient_methods(c, false)?;

                child.scope.super_class = super_class
                    .clone()
                    .map(|ty| instantiate_class(child.ctx.module_id, *ty));
                {
                    // Validate constructors
                    let mut constructor_spans = vec![];
                    let mut constructor_required_param_count = None;
                    let constructors_with_body = c
                        .body
                        .iter()
                        .filter_map(|member| match member {
                            RClassMember::Constructor(c) if c.body.is_some() => Some(c.span),
                            _ => None,
                        })
                        .collect_vec();

                    if constructors_with_body.len() >= 2 {
                        for &span in &constructors_with_body {
                            child.storage.report(Error::DuplciateConstructor { span })
                        }
                    }

                    for m in c.body.iter() {
                        match *m {
                            RClassMember::Constructor(ref cons) => {
                                //
                                if cons.body.is_none() {
                                    for p in &cons.params {
                                        match *p {
                                            RParamOrTsParamProp::TsParamProp(..) => {
                                                child.storage.report(Error::TS2369 { span: p.span() })
                                            }
                                            _ => {}
                                        }
                                    }
                                }

                                if constructors_with_body.len() == 1 {
                                    // Check parameter count
                                    let required_param_count = cons
                                        .params
                                        .iter()
                                        .filter(|p| match p {
                                            RParamOrTsParamProp::Param(RParam {
                                                pat:
                                                    RPat::Ident(RBindingIdent {
                                                        id: RIdent { optional: true, .. },
                                                        ..
                                                    }),
                                                ..
                                            }) => false,
                                            _ => true,
                                        })
                                        .count();

                                    match constructor_required_param_count {
                                        Some(v) if required_param_count != v => {
                                            for span in constructor_spans.drain(..) {
                                                child.storage.report(Error::WrongOverloadSignature { span })
                                            }
                                        }

                                        None => constructor_required_param_count = Some(required_param_count),
                                        _ => {}
                                    }
                                }

                                constructor_spans.push(cons.span);
                            }

                            _ => {}
                        }
                    }
                }

                {
                    // Remove class members with const EnumVariant keys.
                    c.body.iter().for_each(|v| match v {
                        RClassMember::Method(method) => match &method.key {
                            RPropName::Computed(c) => match c.validate_with(child) {
                                Ok(Key::Computed(ComputedKey { ty, .. })) => match ty.normalize() {
                                    Type::EnumVariant(e) => {
                                        //
                                        if let Some(m) = &mut child.mutations {
                                            m.for_class_members.entry(method.node_id).or_default().remove = true;
                                        }
                                    }
                                    _ => {}
                                },
                                _ => {}
                            },
                            _ => {}
                        },

                        _ => {}
                    });
                }

                // Handle nodes in order described above
                let mut body = {
                    let mut declared_static_keys = vec![];
                    let mut declared_instance_keys = vec![];

                    // Handle static properties
                    for (index, node) in c.body.iter().enumerate() {
                        match node {
                            RClassMember::ClassProp(RClassProp { is_static: true, .. })
                            | RClassMember::PrivateProp(RPrivateProp { is_static: true, .. }) => {
                                let m = node.validate_with(child)?;
                                if let Some(member) = m {
                                    // Check for duplicate property names.
                                    if let Some(key) = member.key() {
                                        // TODO: Use better logic for testing key equality
                                        if declared_static_keys.iter().any(|prev: &Key| prev.type_eq(&*key)) {
                                            child.storage.report(Error::DuplicateProperty { span: key.span() })
                                        }
                                        declared_static_keys.push(key.into_owned());
                                    }

                                    let member = member.fold_with(&mut LitGeneralizer);
                                    child.scope.this_class_members.push((index, member));
                                }
                            }
                            _ => {}
                        }
                    }

                    // Handle ts parameter properties
                    for (index, constructor) in c.body.iter().enumerate().filter_map(|(i, member)| match member {
                        RClassMember::Constructor(c) => Some((i, c)),
                        _ => None,
                    }) {
                        for param in &constructor.params {
                            match param {
                                RParamOrTsParamProp::TsParamProp(p) => {
                                    if p.accessibility == Some(Accessibility::Private) {
                                        let is_optional = match p.param {
                                            RTsParamPropParam::Ident(_) => false,
                                            RTsParamPropParam::Assign(_) => true,
                                        };
                                        let mut key = match &p.param {
                                            RTsParamPropParam::Assign(RAssignPat {
                                                left: box RPat::Ident(key),
                                                ..
                                            })
                                            | RTsParamPropParam::Ident(key) => key.clone(),
                                            _ => unreachable!(
                                                "TypeScript parameter property with pattern other than an identifier"
                                            ),
                                        };
                                        key.type_ann = None;
                                        let key = box RExpr::Ident(key.id);
                                        additional_members.push(RClassMember::ClassProp(RClassProp {
                                            node_id: NodeId::invalid(),
                                            span: p.span,
                                            key,
                                            value: None,
                                            is_static: false,
                                            computed: false,
                                            accessibility: Some(Accessibility::Private),
                                            is_abstract: false,
                                            is_optional,
                                            readonly: p.readonly,
                                            definite: false,
                                            type_ann: None,
                                            decorators: Default::default(),
                                            declare: false,
                                            is_override: false,
                                        }));
                                    }

                                    let (i, ty) = match &p.param {
                                        RTsParamPropParam::Ident(i) => {
                                            let mut ty = i.type_ann.clone();
                                            let ty = try_opt!(ty.validate_with(child));
                                            (i, ty)
                                        }
                                        RTsParamPropParam::Assign(RAssignPat {
                                            span,
                                            left: box RPat::Ident(i),
                                            type_ann,
                                            right,
                                            ..
                                        }) => {
                                            let mut ty = type_ann.clone().or_else(|| i.type_ann.clone());
                                            let mut ty = try_opt!(ty.validate_with(child));
                                            if ty.is_none() {
                                                ty = Some(right.validate_with_default(child)?.generalize_lit());
                                            }
                                            (i, ty)
                                        }
                                        _ => unreachable!(),
                                    };

                                    if let Some(ty) = &ty {
                                        if i.type_ann.is_none() {
                                            if let Some(m) = &mut child.mutations {
                                                m.for_pats.entry(i.node_id).or_default().ty = Some(ty.clone());
                                            }
                                        }
                                    }
                                    // Register a class property.

                                    child.scope.this_class_members.push((
                                        index,
                                        ClassMember::Property(stc_ts_types::ClassProperty {
                                            span: p.span,
                                            key: Key::Normal {
                                                span: i.id.span,
                                                sym: i.id.sym.clone(),
                                            },
                                            value: ty.map(Box::new),
                                            is_static: false,
                                            accessibility: p.accessibility,
                                            is_abstract: false,
                                            is_optional: false,
                                            readonly: p.readonly,
                                            definite: false,
                                        }),
                                    ));
                                }
                                RParamOrTsParamProp::Param(..) => {}
                            }
                        }
                    }

                    // Handle properties
                    for (index, member) in c.body.iter().enumerate() {
                        match member {
                            RClassMember::ClassProp(RClassProp { is_static: false, .. })
                            | RClassMember::PrivateProp(RPrivateProp { is_static: false, .. }) => {
                                //
                                let class_member = member.validate_with(child)?;
                                if let Some(member) = class_member {
                                    // Check for duplicate property names.
                                    if let Some(key) = member.key() {
                                        // TODO: Use better logic for testing key equality
                                        if declared_instance_keys.iter().any(|prev: &Key| prev.type_eq(&*key)) {
                                            child.storage.report(Error::DuplicateProperty { span: key.span() })
                                        }
                                        declared_instance_keys.push(key.into_owned());
                                    }

                                    let member = member.fold_with(&mut LitGeneralizer);
                                    child.scope.this_class_members.push((index, member));
                                }
                            }
                            _ => {}
                        }
                    }

                    for (index, constructor) in c.body.iter().enumerate().filter_map(|(i, member)| match member {
                        RClassMember::Constructor(c) => Some((i, c)),
                        _ => None,
                    }) {
                        let member = constructor.validate_with(child)?;
                        child.scope.this_class_members.push((index, member.into()));
                    }

                    // Handle user-declared method signatures.
                    for member in &c.body {}

                    // Handle body of methods and a constructor
                    // (in try mode - we ignores error if it's not related to the type of return
                    // value)
                    //
                    // This is to infer return types of methods
                    for member in &c.body {}

                    // Actaully check types of method / constructors.

                    let remaining = c
                        .body
                        .iter()
                        .enumerate()
                        .filter(|(index, _)| child.scope.this_class_members.iter().all(|(idx, _)| *idx != *index))
                        .map(|v| v.0)
                        .collect::<Vec<_>>();

                    let order = child.calc_order_of_class_methods(remaining, &c.body);

                    for index in order {
                        let ty = c.body[index].validate_with(child)?;
                        if let Some(ty) = ty {
                            child.scope.this_class_members.push((index, ty));
                        }
                    }

                    take(&mut child.scope.this_class_members)
                };

                {
                    // Change types of getter and setter

                    let mut prop_types = PropertyMap::default();

                    for (_, m) in body.iter_mut() {
                        match m {
                            ClassMember::IndexSignature(_) | ClassMember::Constructor(_) => continue,

                            ClassMember::Method(m) => match m.kind {
                                MethodKind::Getter => {
                                    prop_types.insert(m.key.clone(), m.ret_ty.clone());
                                }
                                _ => {}
                            },

                            ClassMember::Property(_) => {}
                        }
                    }

                    for (index, m) in body.iter_mut() {
                        let orig = &c.body[*index];
                        match m {
                            ClassMember::IndexSignature(_) | ClassMember::Constructor(_) => continue,

                            ClassMember::Method(m) => match m.kind {
                                MethodKind::Setter => {
                                    if let Some(param) = m.params.first_mut() {
                                        if param.ty.is_any() {
                                            if let Some(ty) = prop_types.get_prop_name(&m.key) {
                                                let new_ty = ty.clone().generalize_lit();
                                                param.ty = box new_ty.clone();
                                                match orig {
                                                    RClassMember::Method(ref method) => {
                                                        let node_id = method.function.params[0].pat.node_id();
                                                        if let Some(node_id) = node_id {
                                                            if let Some(m) = &mut child.mutations {
                                                                m.for_pats.entry(node_id).or_default().ty =
                                                                    Some(new_ty.clone())
                                                            }
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }
                                }
                                MethodKind::Method => continue,
                                MethodKind::Getter => {}
                            },

                            ClassMember::Property(_) => {}
                        }
                    }
                }

                if !additional_members.is_empty() {
                    // Add private parameter properties to .d.ts file

                    if let Some(m) = &mut child.mutations {
                        m.for_classes
                            .entry(c.node_id)
                            .or_default()
                            .additional_members
                            .extend(additional_members);
                    }
                }

                let class = ClassDef {
                    span: c.span,
                    name,
                    is_abstract: c.is_abstract,
                    super_class,
                    type_params,
                    body: body.into_iter().map(|v| v.1).collect(),
                    implements,
                };

                child.validate_inherited_members_from_super_class(None, &class);
                child.validate_inherited_members_from_interfaces(None, &class);
                child.validate_interface_conflicts(&class.implements);

                Ok(class)
            },
        )?;

        for (i, ty) in types_to_register {
            self.register_type(i, ty);
        }

        Ok(c)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, c: &RClassExpr) -> ValidationResult<()> {
        self.scope.this_class_name = c.ident.as_ref().map(|v| v.into());
        let ty = match c.class.validate_with(self) {
            Ok(ty) => ty.into(),
            Err(err) => {
                self.storage.report(err);
                Type::any(c.span())
            }
        };

        let old_this = self.scope.this.take();
        // self.scope.this = Some(ty.clone());

        let c = self
            .with_child(ScopeKind::Block, Default::default(), |analyzer| {
                if let Some(ref i) = c.ident {
                    let ty = analyzer.register_type(i.into(), ty);

                    match analyzer.declare_var(
                        ty.span(),
                        VarDeclKind::Var,
                        i.into(),
                        Some(ty),
                        None,
                        // initialized = true
                        true,
                        // declare Class does not allow multiple declarations.
                        false,
                        false,
                    ) {
                        Ok(()) => {}
                        Err(err) => {
                            analyzer.storage.report(err);
                        }
                    }
                }

                c.visit_children_with(analyzer);

                Ok(())
            })
            .report(&mut self.storage);

        self.scope.this = old_this;

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, c: &RClassDecl) -> ValidationResult<()> {
        self.record(c);

        let ctx = Ctx {
            in_declare: self.ctx.in_declare || c.declare,
            ..self.ctx
        };
        self.with_ctx(ctx).visit_class_decl_inner(c);

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    fn validate_super_class(&mut self, ty: &Type) {
        if self.is_builtin {
            return;
        }

        let res: ValidationResult<_> = try {
            match ty.normalize() {
                Type::Ref(Ref {
                    type_name: RTsEntityName::Ident(i),
                    ..
                }) => {
                    if let Some(name) = &self.scope.this_class_name {
                        if *name == i {
                            Err(Error::SelfReferentialSuperClass { span: i.span })?
                        }
                    }
                }
                _ => {}
            }

            let ty = self.normalize(None, ty, Default::default())?;

            match ty.normalize() {
                Type::Function(..) => Err(Error::NotConstructorType { span: ty.span() })?,

                _ => {}
            }
        };

        res.report(&mut self.storage);
    }

    /// TODO: Instantate fully
    pub(crate) fn instantiate_class(&mut self, span: Span, ty: &Type) -> ValidationResult {
        Ok(match ty.normalize() {
            Type::ClassDef(def) => Type::Class(Class {
                span,
                def: box def.clone(),
            }),
            _ => ty.clone(),
        })
    }

    fn visit_class_decl_inner(&mut self, c: &RClassDecl) {
        c.ident.visit_with(self);

        self.scope.this_class_name = Some(c.ident.clone().into());
        let ty = match c.class.validate_with(self) {
            Ok(ty) => ty.into(),
            Err(err) => {
                self.storage.report(err);
                Type::any(c.span())
            }
        };
        let ty = ty.cheap();

        let old_this = self.scope.this.take();
        // self.scope.this = Some(ty.clone());

        let ty = self.register_type(c.ident.clone().into(), ty.clone().into());

        match self.declare_var(
            ty.span(),
            VarDeclKind::Var,
            c.ident.clone().into(),
            Some(ty),
            None,
            // initialized = true
            true,
            // declare Class does not allow multiple declarations.
            false,
            false,
        ) {
            Ok(()) => {}
            Err(err) => {
                self.storage.report(err);
            }
        }

        self.scope.this = old_this;
    }
}

fn from_pat(pat: RPat) -> RTsFnParam {
    match pat {
        RPat::Ident(v) => v.into(),
        RPat::Array(v) => v.into(),
        RPat::Rest(v) => v.into(),
        RPat::Object(v) => v.into(),
        RPat::Assign(v) => from_pat(*v.left),
        _ => unreachable!("constructor with parameter {:?}", pat),
    }
}
