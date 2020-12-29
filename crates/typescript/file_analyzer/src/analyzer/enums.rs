use super::Analyzer;
use crate::{
    analyzer::util::ResultExt,
    errors::Error,
    ty::{Enum, EnumMember, Type},
    validator, ValidationResult,
};
use fxhash::FxHashMap;
use rnode::NodeId;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RBinExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RLit;
use stc_ts_ast_rnode::RNumber;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsEnumDecl;
use stc_ts_ast_rnode::RTsEnumMemberId;
use stc_ts_ast_rnode::RTsLit;
use stc_ts_ast_rnode::RTsLitType;
use stc_ts_types::Id;
use swc_atoms::JsWord;
use swc_common::{Span, Spanned};
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
    fn validate(&mut self, e: &mut RTsEnumDecl) -> ValidationResult<Enum> {
        let mut default = 0;
        let mut values = Default::default();
        let ty: Result<_, _> = try {
            let members = e
                .members
                .iter()
                .map(|m| -> Result<_, Error> {
                    let id_span = m.id.span();
                    let val = compute(
                        &e,
                        id_span,
                        &mut values,
                        Some(default),
                        m.init.as_ref().map(|v| &**v),
                    )
                    .map(|val| {
                        match &val {
                            RTsLit::Number(n) => {
                                default = n.value as i32 + 1;
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
                            RTsLit::Tpl(v) => {
                                RExpr::Lit(RLit::Str(v.quasis.into_iter().next().unwrap().raw))
                            }
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

            Enum {
                span: e.span,
                has_num: members.iter().any(|m| match *m.val {
                    RExpr::Lit(RLit::Num(..)) => true,
                    _ => false,
                }),
                has_str: members.iter().any(|m| match *m.val {
                    RExpr::Lit(RLit::Str(..)) => true,
                    _ => false,
                }),
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

        self.register_type(name.clone(), stored_ty.clone())
            .report(&mut self.storage);

        self.declare_var(
            e.span,
            VarDeclKind::Let,
            name.clone(),
            Some(stored_ty),
            true,
            true,
        )
        .report(&mut self.storage);

        // Validate const enums
        if e.is_const {
            for m in &e.members {
                if let Some(init) = &m.init {
                    let mut v = LitValidator {
                        error: false,
                        decl: &e,
                    };
                    init.visit_with(&mut v);
                    if v.error {
                        self.storage
                            .report(Error::InvalidInitInConstEnum { span: init.span() })
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
    default: Option<i32>,
    init: Option<&RExpr>,
) -> Result<RTsLit, Error> {
    fn compute_bin(
        e: &RTsEnumDecl,
        span: Span,
        values: &mut EnumValues,
        expr: &RBinExpr,
    ) -> Result<RTsLit, Error> {
        let l = compute(e, span, values, None, Some(&expr.left))?;
        let r = compute(e, span, values, None, Some(&expr.right))?;

        Ok(match (l, r) {
            (
                RTsLit::Number(RNumber { value: l, .. }),
                RTsLit::Number(RNumber { value: r, .. }),
            ) => {
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
                                return compute(
                                    e,
                                    span,
                                    values,
                                    None,
                                    m.init.as_ref().map(|v| &**v),
                                );
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
    // Check for constant enum in rvalue.
    pub(super) fn check_rvalue(&mut self, rhs_ty: &Type) {
        match *rhs_ty.normalize() {
            Type::Enum(ref e) if e.is_const => {
                self.storage
                    .report(Error::ConstEnumUsedAsVar { span: e.span() });
            }
            _ => {}
        }
    }

    pub(super) fn expand_enum_variant(&self, ty: Type) -> Result<Type, Error> {
        match ty.normalize() {
            Type::EnumVariant(ref v) => {
                if let Some(types) = self.find_type(v.ctxt, &v.enum_name)? {
                    for ty in types {
                        if let Type::Enum(Enum { members, .. }) = ty.normalize() {
                            if let Some(v) = members.iter().find(|m| match m.id {
                                RTsEnumMemberId::Ident(RIdent { ref sym, .. })
                                | RTsEnumMemberId::Str(RStr { value: ref sym, .. }) => {
                                    *sym == v.name
                                }
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
