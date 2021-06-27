use crate::{
    analyzer::{scope::VarKind, util::ResultExt, Analyzer},
    ty::{Enum, EnumMember, Type},
    validator, ValidationResult,
};
use fxhash::FxHashMap;
use rnode::{NodeId, Visit, VisitWith};
use stc_ts_ast_rnode::{
    RBinExpr, RBindingIdent, RExpr, RIdent, RLit, RNumber, RPat, RStr, RTsEnumDecl, RTsEnumMember, RTsEnumMemberId,
    RTsKeywordType, RTsLit, RTsLitType,
};
use stc_ts_errors::Error;
use stc_ts_types::{Accessor, EnumVariant, FnParam, Id, IndexSignature, Key, PropertySignature, TypeElement, TypeLit};
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

/// Value does not contain RTsLit::Bool
type EnumValues = FxHashMap<JsWord, RTsLit>;

/// We don't visit enum variants to allow
///
/// ```ts
///        const enum E {
///            a = 10,
///            b = a,
///            c = (a+1),
///            e,
///            d = ~e,
///            f = a << 2 >> 1,
///            g = a << 2 >>> 1,
///            h = a | b
///        }
/// ```
#[validator]
impl Analyzer<'_, '_> {
    #[inline(never)]
    fn validate(&mut self, e: &RTsEnumDecl) -> ValidationResult<Enum> {
        for m in &e.members {
            self.validate_with(|a| a.validate_enum_memeber_name(&m.id));
        }

        let mut default = 0.0;
        let mut values = Default::default();
        let ty: Result<_, _> = try {
            let members = e
                .members
                .iter()
                .map(|m| -> Result<_, Error> {
                    let id_span = m.id.span();
                    let val = compute(&e, id_span, &mut values, Some(default), m.init.as_ref().map(|v| &**v))
                        .map(|val| {
                            match &val {
                                RTsLit::Number(n) => {
                                    default = n.value + 1.0;
                                }
                                _ => {}
                            }
                            values.insert(
                                match &m.id {
                                    RTsEnumMemberId::Ident(i) => i.sym.clone(),
                                    RTsEnumMemberId::Str(s) => s.value.clone(),
                                },
                                val.clone(),
                            );

                            match val {
                                RTsLit::Number(v) => RExpr::Lit(RLit::Num(v)),
                                RTsLit::Str(v) => RExpr::Lit(RLit::Str(v)),
                                RTsLit::Bool(v) => RExpr::Lit(RLit::Bool(v)),
                                RTsLit::Tpl(v) => RExpr::Lit(RLit::Str(v.quasis.into_iter().next().unwrap().raw)),
                                RTsLit::BigInt(v) => RExpr::Lit(RLit::BigInt(v)),
                            }
                        })
                        .or_else(|err| match &m.init {
                            None => Err(err),
                            Some(v) => Ok(*v.clone()),
                        })?;

                    Ok(EnumMember {
                        id: m.id.clone(),
                        val: box val,
                        span: m.span,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let has_str = members.iter().any(|m| match *m.val {
                RExpr::Lit(RLit::Str(..)) => true,
                _ => false,
            });

            if has_str {
                for m in &e.members {
                    self.validate_member_of_str_enum(m);
                }
            }

            Enum {
                span: e.span,
                has_num: members.iter().any(|m| match *m.val {
                    RExpr::Lit(RLit::Num(..)) => true,
                    _ => false,
                }),
                has_str,
                declare: e.declare,
                is_const: e.is_const,
                id: e.id.clone(),
                members,
            }
        };

        let span = e.span;
        let name = Id::from(&e.id);

        let stored_ty = ty
            .clone()
            .map(Type::Enum)
            .map(Type::cheap)
            .report(&mut self.storage)
            .unwrap_or_else(|| Type::any(span));

        self.register_type(name.clone(), stored_ty.clone());

        self.declare_var(
            e.span,
            VarKind::Enum,
            name.clone(),
            Some(stored_ty),
            None,
            true,
            true,
            false,
        )
        .report(&mut self.storage);

        // Validate const enums
        if e.is_const {
            for m in &e.members {
                if let Some(init) = &m.init {
                    let mut v = LitValidator { error: false, decl: &e };
                    init.visit_with(&mut v);
                    if v.error {
                        self.storage.report(Error::InvalidInitInConstEnum { span: init.span() })
                    }
                }
            }
        }

        ty
    }
}

/// Called only for enums.
///
/// If both of the default value and the initialization is None, this method
/// returns [Err].
fn compute(
    e: &RTsEnumDecl,
    span: Span,
    values: &mut EnumValues,
    default: Option<f64>,
    init: Option<&RExpr>,
) -> Result<RTsLit, Error> {
    fn compute_bin(e: &RTsEnumDecl, span: Span, values: &mut EnumValues, expr: &RBinExpr) -> Result<RTsLit, Error> {
        let l = compute(e, span, values, None, Some(&expr.left))?;
        let r = compute(e, span, values, None, Some(&expr.right))?;

        Ok(match (l, r) {
            (RTsLit::Number(RNumber { value: l, .. }), RTsLit::Number(RNumber { value: r, .. })) => {
                RTsLit::Number(RNumber {
                    span,
                    value: match expr.op {
                        op!(bin, "+") => l + r,
                        op!(bin, "-") => l - r,
                        op!("*") => l * r,
                        op!("/") => l / r,

                        // TODO
                        op!("&") => ((l.round() as i64) & (r.round() as i64)) as _,
                        op!("|") => ((l.round() as i64) | (r.round() as i64)) as _,
                        op!("^") => ((l.round() as i64) ^ (r.round() as i64)) as _,

                        op!("<<") => ((l.round() as i64) << (r.round() as i64)) as _,
                        op!(">>") => ((l.round() as i64) >> (r.round() as i64)) as _,
                        // TODO: Verify this
                        op!(">>>") => ((l.round() as u64) >> (r.round() as u64)) as _,
                        _ => Err(Error::InvalidEnumInit { span })?,
                    },
                })
            }
            (RTsLit::Str(l), RTsLit::Str(r)) if expr.op == op!(bin, "+") => RTsLit::Str(RStr {
                span,
                value: format!("{}{}", l.value, r.value).into(),
                has_escape: l.has_escape || r.has_escape,
                kind: Default::default(),
            }),
            (RTsLit::Number(l), RTsLit::Str(r)) if expr.op == op!(bin, "+") => RTsLit::Str(RStr {
                span,
                value: format!("{}{}", l.value, r.value).into(),
                has_escape: r.has_escape,
                kind: Default::default(),
            }),
            (RTsLit::Str(l), RTsLit::Number(r)) if expr.op == op!(bin, "+") => RTsLit::Str(RStr {
                span,
                value: format!("{}{}", l.value, r.value).into(),
                has_escape: l.has_escape,
                kind: Default::default(),
            }),
            _ => Err(Error::InvalidEnumInit { span })?,
        })
    }

    fn try_str(e: &RExpr) -> Result<RStr, ()> {
        match *e {
            RExpr::Lit(RLit::Str(ref s)) => return Ok(s.clone()),
            _ => Err(()),
        }
    }

    if let Some(expr) = init {
        match expr {
            RExpr::Lit(RLit::Str(s)) => return Ok(RTsLit::Str(s.clone())),
            RExpr::Lit(RLit::Num(s)) => return Ok(RTsLit::Number(s.clone())),
            RExpr::Bin(ref bin) => return compute_bin(e, span, values, &bin),
            RExpr::Paren(ref paren) => return compute(e, span, values, default, Some(&paren.expr)),

            RExpr::Ident(ref id) => {
                if let Some(v) = values.get(&id.sym) {
                    return Ok(v.clone());
                }
                //
                for m in e.members.iter() {
                    match m.id {
                        RTsEnumMemberId::Str(RStr { value: ref sym, .. })
                        | RTsEnumMemberId::Ident(RIdent { ref sym, .. }) => {
                            if *sym == id.sym {
                                return compute(e, span, values, None, m.init.as_ref().map(|v| &**v));
                            }
                        }
                    }
                }
                return Err(Error::InvalidEnumInit { span });
            }
            RExpr::Unary(ref expr) => {
                let v = compute(e, span, values, None, Some(&expr.arg))?;
                match v {
                    RTsLit::Number(RNumber { value: v, .. }) => {
                        return Ok(RTsLit::Number(RNumber {
                            span,
                            value: match expr.op {
                                op!(unary, "+") => v,
                                op!(unary, "-") => -v,
                                op!("!") => {
                                    if v == 0.0f64 {
                                        0.0
                                    } else {
                                        1.0
                                    }
                                }
                                op!("~") => (!(v as i32)) as f64,
                                _ => Err(Error::InvalidEnumInit { span })?,
                            },
                        }))
                    }
                    RTsLit::Str(_) => {}
                    RTsLit::Bool(_) => {}
                    RTsLit::Tpl(_) => {}
                    RTsLit::BigInt(_) => unimplemented!("BigInt in enum"),
                }
            }

            RExpr::Tpl(ref t) if t.exprs.is_empty() => {
                if let Some(v) = &t.quasis[0].cooked {
                    return Ok(v.clone().into());
                }
            }

            _ => {}
        }
    } else {
        if let Some(value) = default {
            return Ok(RTsLit::Number(RNumber {
                span,
                value: value as _,
            }));
        }
    }

    Err(Error::InvalidEnumInit { span })
}

impl Analyzer<'_, '_> {
    fn validate_enum_memeber_name(&mut self, e: &RTsEnumMemberId) -> ValidationResult<()> {
        match e {
            RTsEnumMemberId::Ident(i) => {}
            RTsEnumMemberId::Str(s) => {
                if s.value.starts_with(|c: char| c.is_digit(10)) {
                    Err(Error::EnumMemberIdCannotBeNumber { span: s.span })?
                }
            }
        }

        Ok(())
    }

    /// `enumBasics.ts` says
    ///
    /// > Enum object type is anonymous with properties of the enum type and
    /// numeric indexer.
    ///
    /// and following is valid.
    ///
    /// ```ts
    /// var e = E1;
    /// var e: {
    ///     readonly A: E1.A;
    ///     readonly B: E1.B;
    ///     readonly C: E1.C;
    ///     readonly [n: number]: string;
    /// };
    /// var e: typeof E1;
    /// ```
    pub(super) fn enum_to_type_lit(&mut self, e: &Enum) -> ValidationResult<TypeLit> {
        let mut members = vec![];

        for m in &e.members {
            let key = match &m.id {
                RTsEnumMemberId::Ident(i) => i.clone(),
                RTsEnumMemberId::Str(s) => RIdent::new(s.value.clone(), s.span),
            };

            members.push(TypeElement::Property(PropertySignature {
                span: m.span,
                accessibility: None,
                readonly: true,
                key: Key::Normal {
                    span: key.span,
                    sym: key.sym.clone(),
                },
                optional: false,
                params: Default::default(),
                type_ann: Some(box Type::EnumVariant(EnumVariant {
                    span: m.span,
                    // TODO: Store context in `Enum`
                    ctxt: self.ctx.module_id,
                    enum_name: e.id.clone().into(),
                    name: Some(key.sym),
                })),
                type_params: Default::default(),
                metadata: Default::default(),
                accessor: Accessor {
                    getter: true,
                    setter: false,
                },
            }))
        }
        {
            let param = FnParam {
                span: DUMMY_SP,
                pat: RPat::Ident(RBindingIdent {
                    node_id: NodeId::invalid(),
                    id: RIdent::new("n".into(), DUMMY_SP),
                    type_ann: None,
                }),
                required: true,
                ty: box Type::Keyword(RTsKeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                }),
            };
            members.push(TypeElement::Index(IndexSignature {
                span: e.span,
                readonly: false,
                params: vec![param],
                type_ann: Some(box Type::Keyword(RTsKeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                })),
                is_static: false,
            }));
        }

        Ok(TypeLit {
            span: e.span,
            members,
            metadata: Default::default(),
        })
    }

    // Check for rvalue of assignments.
    pub(super) fn check_rvalue(&mut self, span: Span, lhs: &RPat, rhs_ty: &Type) {
        match *rhs_ty.normalize() {
            // Report an error for `a = G` where G is name of the const enum itself.
            Type::Enum(ref e) if e.is_const => {
                self.storage.report(Error::InvalidUseOfConstEnum { span });
            }
            Type::Keyword(RTsKeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => {
                if self.rule().strict_null_checks {
                    match lhs {
                        RPat::Array(_) | RPat::Rest(_) | RPat::Object(_) => {
                            self.storage.report(Error::ObjectIsPossiblyUndefined { span });
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn validate_member_of_str_enum(&mut self, m: &RTsEnumMember) {
        fn type_of_expr(e: &RExpr) -> Option<swc_ecma_utils::Type> {
            Some(match e {
                RExpr::Lit(RLit::Str(..)) => swc_ecma_utils::Type::Str,
                RExpr::Tpl(t) => {
                    if t.exprs.is_empty() {
                        swc_ecma_utils::Type::Str
                    } else {
                        return None;
                    }
                }
                RExpr::Lit(RLit::Num(..)) => swc_ecma_utils::Type::Num,
                RExpr::Bin(RBinExpr {
                    op: op!(bin, "+"),
                    left,
                    right,
                    ..
                }) => {
                    let lt = type_of_expr(&left)?;
                    let rt = type_of_expr(&right)?;
                    if lt == rt && lt == swc_ecma_utils::Type::Str {
                        return Some(lt);
                    }

                    return None;
                }
                _ => return None,
            })
        }

        match m.init.as_deref() {
            Some(RExpr::Ident(..)) => {}
            Some(e) => {
                if type_of_expr(&e).is_none() {
                    self.storage
                        .report(Error::ComputedMemberInEnumWithStrMember { span: m.span })
                }
            }
            _ => {}
        }
    }

    /// Converts `e` in `o[e]` from the code below to `'a' | 'b'`
    ///
    /// ```ts
    /// enum E {
    ///     A = 'a',
    ///     B = 'b',
    /// }
    ///
    /// const o = { a: 1, b: 2 };
    /// declare const e: E;
    /// const a = o[e]
    /// ```
    pub(super) fn expand_enum(&mut self, ty: Type) -> ValidationResult {
        let e = match ty.normalize() {
            Type::Enum(e) => e,
            _ => return Ok(ty),
        };

        let mut values = vec![];

        for m in &e.members {
            match &*m.val {
                RExpr::Lit(RLit::Str(lit)) => values.push(Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    span: m.span,
                    lit: RTsLit::Str(lit.clone()),
                })),
                RExpr::Lit(RLit::Num(lit)) => values.push(Type::Lit(RTsLitType {
                    node_id: NodeId::invalid(),
                    span: m.span,
                    lit: RTsLit::Number(lit.clone()),
                })),
                _ => {
                    unimplemented!("Handle enum with value other than string literal or numeric literals")
                }
            }
        }

        let mut ty = Type::union(values);
        ty.reposition(e.span);

        Ok(ty)
    }

    pub(super) fn expand_enum_variant(&self, ty: Type) -> ValidationResult {
        match ty.normalize() {
            Type::EnumVariant(ref v) => {
                if let Some(variant_name) = &v.name {
                    if let Some(types) = self.find_type(v.ctxt, &v.enum_name)? {
                        for ty in types {
                            if let Type::Enum(Enum { members, .. }) = ty.normalize() {
                                if let Some(v) = members.iter().find(|m| match m.id {
                                    RTsEnumMemberId::Ident(RIdent { ref sym, .. })
                                    | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => sym == variant_name,
                                }) {
                                    match *v.val {
                                        RExpr::Lit(RLit::Str(..)) | RExpr::Lit(RLit::Num(..)) => {
                                            return Ok(Type::Lit(RTsLitType {
                                                node_id: NodeId::invalid(),
                                                span: v.span,
                                                lit: match *v.val.clone() {
                                                    RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                                    RExpr::Lit(RLit::Num(n)) => RTsLit::Number(n),
                                                    _ => unreachable!(),
                                                },
                                            }));
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        return Ok(ty);
    }
}

struct LitValidator<'a> {
    decl: &'a RTsEnumDecl,
    error: bool,
}

impl Visit<RExpr> for LitValidator<'_> {
    fn visit(&mut self, e: &RExpr) {
        e.visit_children_with(self);

        match e {
            RExpr::Lit(..) => {}
            RExpr::Ident(ref i) => {
                if i.sym == js_word!("NaN") || i.sym == js_word!("Infinity") {
                    return;
                }

                let is_ref = self.decl.members.iter().any(|m| match m.id {
                    RTsEnumMemberId::Ident(RIdent { ref sym, .. })
                    | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => *sym == i.sym,
                });
                if !is_ref {
                    self.error = true;
                    return;
                }
            }
            RExpr::Unary(..) | RExpr::Bin(..) | RExpr::Paren(..) => {}

            _ => {
                self.error = true;
                return;
            }
        }
    }
}
