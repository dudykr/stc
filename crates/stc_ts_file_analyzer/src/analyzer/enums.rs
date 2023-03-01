use fxhash::FxHashMap;
use rnode::{NodeId, Visit, VisitWith};
use stc_ts_ast_rnode::{
    RBinExpr, RBindingIdent, RExpr, RIdent, RLit, RNumber, RPat, RStr, RTsEnumDecl, RTsEnumMember, RTsEnumMemberId, RTsLit,
};
use stc_ts_errors::{ErrorKind, Errors};
use stc_ts_file_analyzer_macros::validator;
use stc_ts_types::{
    Accessor, ArcCowType, EnumVariant, FnParam, Id, IndexSignature, Key, KeywordType, LitType, LitTypeMetadata, PropertySignature,
    TypeElement, TypeLit,
};
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

use crate::{
    analyzer::{scope::VarKind, util::ResultExt, Analyzer},
    ty::{Enum, EnumMember, Type},
    validator::ValidateWith,
    VResult,
};

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
    fn validate(&mut self, e: &RTsEnumDecl) -> VResult<Enum> {
        for m in &e.members {
            self.validate_with(|a| a.validate_enum_member_name(&m.id));
        }

        let mut default = 0.0;
        let mut values = Default::default();

        let mut eval = Evaluator {
            e,
            values: &mut values,
            errors: Default::default(),
        };

        let ty: Result<_, _> = try {
            let members = e
                .members
                .iter()
                .map(|m| -> VResult<_> {
                    let id_span = m.id.span();
                    let val = eval
                        .compute(self, id_span, Some(default), m.init.as_deref())
                        .map(|val| {
                            if let RTsLit::Number(n) = &val {
                                default = n.value + 1.0;
                            }
                            eval.values.insert(
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
                                RTsLit::Tpl(v) => RExpr::Lit(RLit::Str(RStr {
                                    span: v.span,
                                    value: From::from(&*v.quasis.into_iter().next().unwrap().raw),
                                    raw: None,
                                })),
                                RTsLit::BigInt(v) => RExpr::Lit(RLit::BigInt(v)),
                            }
                        })
                        .or_else(|err| match &m.init {
                            None => Err(err),
                            Some(v) => {
                                if e.is_const {
                                    self.storage.report(err);
                                }
                                Ok(*v.clone())
                            }
                        })?;

                    Ok(EnumMember {
                        id: m.id.clone(),
                        val: box val,
                        span: m.span,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            if e.members.iter().any(|m| matches!(m.init, Some(box RExpr::Lit(RLit::Str(..))))) {
                for m in e.members.iter() {
                    if let Some(box (RExpr::Unary(..) | RExpr::Bin(..) | RExpr::Member(..))) = m.init {
                        if let Some(box RExpr::Bin(bin)) = &m.init {
                            if bin.op == op!(bin, "+") {
                                if non_str_nor_plus(bin) {
                                    continue;
                                }
                            }
                        }
                        self.storage
                            .report(ErrorKind::ComputedMemberInEnumWithStrMember { span: m.span }.into());
                    }
                }
            }

            let has_str = members.iter().any(|m| matches!(*m.val, RExpr::Lit(RLit::Str(..))));

            if has_str {
                for m in &e.members {
                    self.validate_member_of_str_enum(m);
                }
            }

            Enum {
                span: e.span,
                has_num: members.iter().any(|m| matches!(*m.val, RExpr::Lit(RLit::Num(..)))),
                has_str,
                declare: e.declare,
                is_const: e.is_const,
                id: e.id.clone(),
                members,
                metadata: Default::default(),
                tracker: Default::default(),
            }
        };

        let span = e.span;
        let name = Id::from(&e.id);

        let stored_ty = ty
            .clone()
            .map(Type::Enum)
            .map(Type::into_freezed)
            .report(&mut self.storage)
            .unwrap_or_else(|| Type::any(span, Default::default()));

        self.register_type(name.clone(), stored_ty.clone());

        self.declare_var(e.span, VarKind::Enum, name, Some(stored_ty), None, true, true, false, false)
            .report(&mut self.storage);

        // Validate const enums
        if e.is_const {
            for m in &e.members {
                if let Some(init) = &m.init {
                    let mut v = LitValidator {
                        error: false,
                        decl: e,
                        errors: Default::default(),
                    };
                    init.visit_with(&mut v);
                    self.storage.report_all(v.errors);
                    if v.error {
                        self.storage.report(ErrorKind::InvalidInitInConstEnum { span: init.span() }.into())
                    }
                }
            }
        }

        ty
    }
}

struct Evaluator<'a> {
    e: &'a RTsEnumDecl,
    values: &'a mut EnumValues,

    #[allow(unused)]
    errors: Errors,
}

impl Evaluator<'_> {
    /// Called only for enums.
    ///
    /// If both of the default value and the initialization is None, this method
    /// returns [Err].
    fn compute(&mut self, analyzer: &mut Analyzer, span: Span, default: Option<f64>, init: Option<&RExpr>) -> VResult<RTsLit> {
        if let Some(expr) = init {
            match expr {
                RExpr::Lit(RLit::Str(s)) => return Ok(RTsLit::Str(s.clone())),
                RExpr::Lit(RLit::Num(s)) => return Ok(RTsLit::Number(s.clone())),
                RExpr::Bin(ref bin) => {
                    let v = self.compute_bin(analyzer, span, bin)?;

                    match &v {
                        RTsLit::Number(n) => {
                            if n.value.is_infinite() && self.e.is_const {
                                return Err(ErrorKind::ConstEnumMemberHasInfinityAsInit { span: bin.span }.into());
                            } else if n.value.is_nan() && self.e.is_const {
                                return Err(ErrorKind::ConstEnumMemberHasNaNAsInit { span: bin.span }.into());
                            } else {
                                return Ok(v);
                            }
                        }
                        _ => return Ok(v),
                    }
                }
                RExpr::Paren(ref paren) => return self.compute(analyzer, span, default, Some(&paren.expr)),

                RExpr::Ident(ref id) => {
                    if self.e.is_const {
                        if id.sym == js_word!("NaN") {
                            return Err(ErrorKind::ConstEnumMemberHasNaNAsInit { span: id.span }.into());
                        }
                        if id.sym == js_word!("Infinity") {
                            return Err(ErrorKind::ConstEnumMemberHasInfinityAsInit { span: id.span }.into());
                        }
                    }

                    if let Some(v) = self.values.get(&id.sym) {
                        return Ok(v.clone());
                    }
                    //
                    for m in self.e.members.iter() {
                        match m.id {
                            RTsEnumMemberId::Str(RStr { value: ref sym, .. }) | RTsEnumMemberId::Ident(RIdent { ref sym, .. }) => {
                                if *sym == id.sym {
                                    return self.compute(analyzer, span, None, m.init.as_deref());
                                }
                            }
                        }
                    }
                    return Err(ErrorKind::InvalidEnumInit { span }.into());
                }
                RExpr::Unary(ref expr) => {
                    let v = self.compute(analyzer, span, None, Some(&expr.arg))?;
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
                                    _ => Err(ErrorKind::InvalidEnumInit { span })?,
                                },
                                raw: None,
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

                _ => {
                    let res = expr.validate_with_default(analyzer)?;
                    let res = analyzer.expand_enum_variant(res)?;

                    if let Type::Lit(ty) = res {
                        return Ok(ty.lit.clone());
                    }
                }
            }
        } else {
            if let Some(value) = default {
                return Ok(RTsLit::Number(RNumber {
                    span,
                    value: value as _,
                    raw: None,
                }));
            }
        }

        Err(ErrorKind::InvalidEnumInit { span }.into())
    }

    fn compute_bin(&mut self, analyzer: &mut Analyzer, span: Span, expr: &RBinExpr) -> VResult<RTsLit> {
        let l = self.compute(analyzer, span, None, Some(&expr.left))?;
        let r = self.compute(analyzer, span, None, Some(&expr.right))?;

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
                        // TODO(kdy1): Verify this
                        op!(">>>") => ((l.round() as u64) >> (r.round() as u64)) as _,
                        _ => Err(ErrorKind::InvalidEnumInit { span })?,
                    },

                    raw: None,
                })
            }
            (RTsLit::Str(l), RTsLit::Str(r)) if expr.op == op!(bin, "+") => RTsLit::Str(RStr {
                span,
                value: format!("{}{}", l.value, r.value).into(),
                raw: None,
            }),
            (RTsLit::Number(l), RTsLit::Str(r)) if expr.op == op!(bin, "+") => RTsLit::Str(RStr {
                span,
                value: format!("{}{}", l.value, r.value).into(),
                raw: None,
            }),
            (RTsLit::Str(l), RTsLit::Number(r)) if expr.op == op!(bin, "+") => RTsLit::Str(RStr {
                span,
                value: format!("{}{}", l.value, r.value).into(),
                raw: None,
            }),
            _ => Err(ErrorKind::InvalidEnumInit { span })?,
        })
    }

    #[allow(unused)]
    fn try_str(e: &RExpr) -> Result<RStr, ()> {
        match *e {
            RExpr::Lit(RLit::Str(ref s)) => Ok(s.clone()),
            _ => Err(()),
        }
    }
}

impl Analyzer<'_, '_> {
    fn validate_enum_member_name(&mut self, e: &RTsEnumMemberId) -> VResult<()> {
        match e {
            RTsEnumMemberId::Ident(i) => {}
            RTsEnumMemberId::Str(s) => {
                if s.value.starts_with(|c: char| c.is_ascii_digit()) {
                    Err(ErrorKind::EnumMemberIdCannotBeNumber { span: s.span })?
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
    pub(super) fn enum_to_type_lit(&mut self, e: &Enum) -> VResult<TypeLit> {
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
                type_ann: Some(
                    Type::EnumVariant(EnumVariant {
                        span: m.span,
                        enum_name: e.id.clone().into(),
                        name: Some(key.sym),
                        metadata: Default::default(),
                        tracker: Default::default(),
                    })
                    .into(),
                ),
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
                ty: box Type::Keyword(KeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }),
            };
            members.push(TypeElement::Index(IndexSignature {
                span: e.span,
                readonly: false,
                params: vec![param],
                type_ann: Some(box Type::Keyword(KeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                })),
                is_static: false,
            }));
        }
        {
            let param = FnParam {
                span: DUMMY_SP,
                pat: RPat::Ident(RBindingIdent {
                    node_id: NodeId::invalid(),
                    id: RIdent::new("s".into(), DUMMY_SP),
                    type_ann: None,
                }),
                required: true,
                ty: box Type::Keyword(KeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                }),
            };
            members.push(TypeElement::Index(IndexSignature {
                span: e.span,
                readonly: false,
                params: vec![param],
                type_ann: Some(box Type::Keyword(KeywordType {
                    span: DUMMY_SP,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    metadata: Default::default(),
                    tracker: Default::default(),
                })),
                is_static: false,
            }));
        }

        Ok(TypeLit {
            span: e.span,
            members,
            metadata: Default::default(),
            tracker: Default::default(),
        })
    }

    // Check for rvalue of assignments.
    pub(super) fn report_error_for_invalid_rvalue(&mut self, span: Span, lhs: &RPat, rhs_ty: &Type) {
        match rhs_ty {
            // Report an error for `a = G` where G is name of the const enum itself.
            Type::Enum(ref e) if e.is_const => {
                self.storage.report(ErrorKind::InvalidUseOfConstEnum { span }.into());
            }
            Type::Keyword(KeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => {
                if self.rule().strict_null_checks {
                    match lhs {
                        RPat::Array(_) | RPat::Rest(_) | RPat::Object(_) => {
                            self.storage.report(ErrorKind::ObjectIsPossiblyUndefined { span }.into());
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
                    let lt = type_of_expr(left)?;
                    let rt = type_of_expr(right)?;
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
                if type_of_expr(e).is_none() && !matches!(e, RExpr::Tpl(..) | RExpr::Bin(..) | RExpr::Member(..)) {
                    self.storage
                        .report(ErrorKind::ComputedMemberInEnumWithStrMember { span: m.span }.into())
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
    pub(super) fn expand_enum(&self, ty: ArcCowType) -> VResult<ArcCowType> {
        let e = match ty {
            Type::Enum(e) => e,
            _ => return Ok(ty),
        };

        let mut values = vec![];

        for m in &e.members {
            match &*m.val {
                RExpr::Lit(RLit::Str(lit)) => values.push(Type::Lit(LitType {
                    span: m.span,
                    lit: RTsLit::Str(lit.clone()),
                    metadata: Default::default(),
                    tracker: Default::default(),
                })),
                RExpr::Lit(RLit::Num(lit)) => values.push(Type::Lit(LitType {
                    span: m.span,
                    lit: RTsLit::Number(lit.clone()),
                    metadata: Default::default(),
                    tracker: Default::default(),
                })),
                _ => {
                    unimplemented!("Handle enum with value other than string literal or numeric literals")
                }
            }
        }
        let span = ty.span();

        let mut ty = Type::new_union(span, values);
        ty.reposition(e.span);

        Ok(ty)
    }

    /// Expands an enum variant as a literal.
    pub(super) fn expand_enum_variant(&self, ty: ArcCowType) -> VResult<ArcCowType> {
        if let Type::EnumVariant(ref ev) = &*ty {
            if let Some(variant_name) = &ev.name {
                if let Some(types) = self.find_type(&ev.enum_name)? {
                    for ty in types {
                        if let Type::Enum(Enum { members, .. }) = &*ty {
                            if let Some(v) = members.iter().find(|m| match m.id {
                                RTsEnumMemberId::Ident(RIdent { ref sym, .. }) | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => {
                                    sym == variant_name
                                }
                            }) {
                                if let RExpr::Lit(RLit::Str(..)) | RExpr::Lit(RLit::Num(..)) = *v.val {
                                    return Ok(Type::Lit(LitType {
                                        span: v.span,
                                        lit: match *v.val.clone() {
                                            RExpr::Lit(RLit::Str(s)) => RTsLit::Str(s),
                                            RExpr::Lit(RLit::Num(n)) => RTsLit::Number(n),
                                            _ => unreachable!(),
                                        },
                                        metadata: LitTypeMetadata {
                                            common: ev.metadata.common,
                                            ..Default::default()
                                        },
                                        tracker: Default::default(),
                                    })
                                    .into());
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(ty)
    }
}

struct LitValidator<'a> {
    decl: &'a RTsEnumDecl,
    error: bool,
    errors: Errors,
}

impl Visit<RExpr> for LitValidator<'_> {
    fn visit(&mut self, e: &RExpr) {
        match e {
            RExpr::Lit(..) => {}
            RExpr::Ident(ref i) => {
                if i.sym == js_word!("NaN") {
                    return;
                }
                if i.sym == js_word!("Infinity") {
                    return;
                }

                let is_ref = self.decl.members.iter().any(|m| match m.id {
                    RTsEnumMemberId::Ident(RIdent { ref sym, .. }) | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => *sym == i.sym,
                });
                if !is_ref {
                    self.error = true;
                }
            }
            RExpr::Member(..) => {}
            RExpr::Unary(..) | RExpr::Bin(..) | RExpr::Paren(..) => {
                e.visit_children_with(self);
            }

            _ => {
                self.error = true;
            }
        }
    }
}

fn non_str_nor_plus(bin: &RBinExpr) -> bool {
    if bin.op == op!(bin, "+") {
        match (&bin.left, &bin.right) {
            (box RExpr::Lit(RLit::Str(..)), box RExpr::Lit(RLit::Str(..))) => return true,
            (box RExpr::Bin(bin), box RExpr::Lit(RLit::Str(..))) => return non_str_nor_plus(bin),
            _ => {}
        }
    }
    false
}
