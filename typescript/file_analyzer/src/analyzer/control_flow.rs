use super::util::ResultExt;
use super::Ctx;
use super::{
    expr::TypeOfMode,
    marks::MarkExt,
    scope::{ScopeKind, VarInfo},
    Analyzer,
};
use crate::util::type_ext::TypeVecExt;
use crate::{
    ty::{Tuple, Type, TypeElement, TypeLit},
    type_facts::TypeFacts,
    util::EndsWithRet,
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::FxHashMap;
use rnode::NodeId;
use rnode::VisitWith;
use stc_ts_ast_rnode::RBinExpr;
use stc_ts_ast_rnode::RCondExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIfStmt;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPatOrExpr;
use stc_ts_ast_rnode::RSwitchCase;
use stc_ts_ast_rnode::RSwitchStmt;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_types::name::Name;
use stc_ts_types::Array;
use stc_ts_types::Id;
use stc_ts_utils::find_ids_in_pat;
use stc_ts_utils::MapWithMut;
use std::borrow::Cow;
use std::{
    collections::hash_map::Entry,
    hash::Hash,
    mem::{replace, take},
    ops::{AddAssign, BitOr, Not},
};
use swc_common::TypeEq;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

/// Conditional facts
#[derive(Debug, Clone, Default)]
pub(crate) struct CondFacts {
    pub facts: FxHashMap<Name, TypeFacts>,
    pub vars: FxHashMap<Name, Type>,
    pub excludes: FxHashMap<Name, Vec<Type>>,
    pub types: FxHashMap<Id, Type>,
}

impl CondFacts {
    pub fn take(&mut self) -> Self {
        Self {
            facts: take(&mut self.facts),
            vars: take(&mut self.vars),
            excludes: take(&mut self.excludes),
            types: take(&mut self.types),
        }
    }

    fn clear(&mut self) {
        self.facts.clear();
        self.vars.clear();
        self.excludes.clear();
        self.types.clear();
    }

    fn extend(&mut self, other: Self) {
        self.facts.extend(other.facts);
        self.vars.extend(other.vars);
        self.types.extend(other.types);
    }

    fn or<K, T>(mut map: FxHashMap<K, T>, map2: FxHashMap<K, T>) -> FxHashMap<K, T>
    where
        K: Eq + Hash,
        T: Merge,
    {
        for (k, v) in map2 {
            match map.entry(k) {
                Entry::Occupied(mut e) => {
                    e.get_mut().or(v);
                }
                Entry::Vacant(e) => {
                    e.insert(v);
                }
            }
        }

        map
    }
}

#[derive(Debug, Default, Clone)]
pub(super) struct Facts {
    pub true_facts: CondFacts,
    pub false_facts: CondFacts,
}

impl Facts {
    pub fn clear(&mut self) {
        self.true_facts.clear();
        self.false_facts.clear();
    }

    pub fn take(&mut self) -> Self {
        Self {
            true_facts: self.true_facts.take(),
            false_facts: self.false_facts.take(),
        }
    }
}

impl Not for Facts {
    type Output = Self;
    #[inline]
    fn not(self) -> Self {
        Facts {
            true_facts: self.false_facts,
            false_facts: self.true_facts,
        }
    }
}

impl AddAssign for Facts {
    fn add_assign(&mut self, rhs: Self) {
        self.true_facts += rhs.true_facts;
        self.false_facts += rhs.false_facts;
    }
}

impl AddAssign<Option<Self>> for Facts {
    fn add_assign(&mut self, rhs: Option<Self>) {
        match rhs {
            Some(rhs) => {
                *self += rhs;
            }
            None => {}
        }
    }
}

impl BitOr for Facts {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Facts {
            true_facts: self.true_facts | rhs.true_facts,
            false_facts: self.false_facts | rhs.false_facts,
        }
    }
}

trait Merge {
    fn or(&mut self, other: Self);
}

impl<T> Merge for Box<T>
where
    T: Merge,
{
    fn or(&mut self, other: Self) {
        T::or(&mut **self, *other)
    }
}

impl<T> Merge for Vec<T> {
    fn or(&mut self, other: Self) {
        self.extend(other)
    }
}

impl Merge for TypeFacts {
    fn or(&mut self, other: Self) {
        *self |= other
    }
}

impl Merge for VarInfo {
    fn or(&mut self, other: Self) {
        self.copied |= other.copied;
        self.initialized |= other.initialized;
        Merge::or(&mut self.ty, other.ty);
    }
}

impl Merge for Type {
    fn or(&mut self, r: Self) {
        let l_span = self.span();

        let l = replace(self, Type::never(l_span));

        *self = Type::union(vec![l, r]);
    }
}

impl<T> Merge for Option<T>
where
    T: Merge,
{
    fn or(&mut self, other: Self) {
        match *self {
            Some(ref mut v) => match other {
                Some(other) => v.or(other),
                None => {}
            },
            _ => *self = other,
        }
    }
}

impl AddAssign for CondFacts {
    fn add_assign(&mut self, rhs: Self) {
        for (k, v) in rhs.facts {
            *self.facts.entry(k).or_insert(TypeFacts::None) |= v;
        }

        self.types.extend(rhs.types);
        self.vars.extend(rhs.vars);
        self.excludes.extend(rhs.excludes);
    }
}

impl AddAssign<Option<Self>> for CondFacts {
    fn add_assign(&mut self, rhs: Option<Self>) {
        match rhs {
            Some(rhs) => {
                *self += rhs;
            }
            None => {}
        }
    }
}

impl BitOr for CondFacts {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        CondFacts {
            facts: CondFacts::or(self.facts, rhs.facts),
            vars: CondFacts::or(self.vars, rhs.vars),
            types: CondFacts::or(self.types, rhs.types),
            excludes: CondFacts::or(self.excludes, rhs.excludes),
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, stmt: &RIfStmt) -> ValidationResult<()> {
        {
            let ctx = Ctx {
                in_cond: true,
                should_store_truthy_for_access: true,
                ..self.ctx
            };
            let _test = stmt.test.validate_with_default(&mut *self.with_ctx(ctx))?;
        }

        let true_facts = self.cur_facts.true_facts.take();
        let false_facts = self.cur_facts.false_facts.take();

        let ends_with_ret = stmt.cons.ends_with_ret();
        self.with_child(ScopeKind::Flow, true_facts, |child| stmt.cons.validate_with(child))?;

        if let Some(alt) = &stmt.alt {
            self.with_child(ScopeKind::Flow, false_facts.clone(), |child| alt.validate_with(child))?;
        }

        if ends_with_ret {
            self.scope.facts.extend(false_facts);
        }

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    /// This method may remove `SafeSubscriber` from `Subscriber` |
    /// `SafeSubscriber` or downgrade the type, like converting `Subscriber` |
    /// `SafeSubscriber` into `SafeSubscriber`. This behavior is controlled by
    /// the mark applied while handling type facts related to call.
    fn adjust_ternary_type(&mut self, span: Span, mut types: Vec<Type>) -> ValidationResult<Vec<Type>> {
        types.iter_mut().for_each(|ty| {
            // Tuple -> Array
            match ty.normalize_mut() {
                Type::Tuple(tuple) => {
                    let span = tuple.span;

                    let mut elem_types: Vec<_> = tuple.elems.take().into_iter().map(|elem| *elem.ty).collect();
                    elem_types.dedup_type();
                    let elem_type = box Type::union(elem_types);
                    *ty = Type::Array(Array { span, elem_type });
                }
                _ => {}
            }
        });

        let should_preserve = types
            .iter()
            .flat_map(|ty| ty.iter_union())
            .flat_map(|ty| ty.iter_union())
            .any(|ty| self.env.shared().marks().prevent_converting_to_children.is_marked(&ty));

        if should_preserve {
            return self.remove_child_types(span, types);
        }

        self.downcast_types(span, types)
    }

    fn downcast_types(&mut self, span: Span, types: Vec<Type>) -> ValidationResult<Vec<Type>> {
        fn need_work(ty: &Type) -> bool {
            match ty.normalize() {
                Type::Lit(..)
                | Type::Keyword(RTsKeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => false,
                _ => true,
            }
        }

        let mut new = vec![];

        'outer: for (ai, ty) in types.iter().flat_map(|ty| ty.iter_union()).enumerate() {
            if need_work(&ty) {
                for (bi, b) in types.iter().flat_map(|ty| ty.iter_union()).enumerate() {
                    if ai == bi || !need_work(&b) {
                        continue;
                    }

                    // If type is same, we need to add it.
                    if b.type_eq(&ty) {
                        break;
                    }

                    match self.extends(span, &b, ty) {
                        Some(true) => {
                            // Remove ty.
                            continue 'outer;
                        }
                        res => {}
                    }
                }
            }

            new.push(ty.clone());
        }

        Ok(new)
    }

    /// Remove `SafeSubscriber` from `Subscriber` | `SafeSubscriber`.
    fn remove_child_types(&mut self, span: Span, types: Vec<Type>) -> ValidationResult<Vec<Type>> {
        let mut new = vec![];

        'outer: for (ai, ty) in types
            .iter()
            .flat_map(|ty| ty.iter_union())
            .flat_map(|ty| ty.iter_union())
            .enumerate()
        {
            for (bi, b) in types.iter().enumerate() {
                if ai == bi {
                    continue;
                }

                match self.extends(span, &ty, b) {
                    Some(true) => {
                        // Remove ty.
                        continue 'outer;
                    }
                    res => {}
                }
            }

            new.push(ty.clone());
        }

        Ok(new)
    }

    fn check_switch_discriminant(&mut self, s: &RSwitchStmt) -> ValidationResult {
        let discriminant_ty = s.discriminant.validate_with_default(self)?;
        for case in &s.cases {
            if let Some(test) = &case.test {
                let case_ty = test.validate_with_default(self)?;
                // self.assign(&discriminant_ty, &case_ty, test.span())
                //     .context("tried to assign the discriminant of switch to
                // the test of a case")     .report(&mut
                // self.storage);
            }
        }

        Ok(discriminant_ty)
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, stmt: &RSwitchStmt) -> ValidationResult<()> {
        self.record(stmt);

        let discriminant_ty = self.check_switch_discriminant(stmt).report(&mut self.storage);

        let mut false_facts = CondFacts::default();
        let mut true_facts = CondFacts::default();
        // Declared at here as it's important to know if last one ends with return.
        let mut ends_with_ret = false;
        let len = stmt.cases.len();
        let stmt_span = stmt.span();

        let mut errored = false;
        // Check cases *in order*
        for (i, case) in stmt.cases.iter().enumerate() {
            if errored {
                break;
            }

            let span = case.test.as_ref().map(|v| v.span()).unwrap_or_else(|| stmt_span);

            let RSwitchCase { cons, .. } = case;
            let last = i == len - 1;

            ends_with_ret = cons.ends_with_ret();

            match case.test {
                Some(ref test) => {
                    let mut binary_test_expr = RExpr::Bin(RBinExpr {
                        node_id: NodeId::invalid(),
                        op: op!("==="),
                        span,
                        left: stmt.discriminant.clone(),
                        right: test.clone(),
                    });
                    let ctx = Ctx {
                        in_cond: true,
                        in_switch_case_test: true,
                        should_store_truthy_for_access: true,
                        ..self.ctx
                    };
                    let mut a = self.with_ctx(ctx);
                    match binary_test_expr.validate_with_default(&mut *a) {
                        Ok(..) => {}
                        Err(err) => {
                            a.storage.report(err);
                            errored = true;
                            continue;
                        }
                    }
                }
                None => {}
            }

            true_facts = true_facts | self.cur_facts.true_facts.take();
            self.with_child(ScopeKind::Flow, true_facts.clone(), |child| {
                cons.visit_with(child);
                Ok(())
            })?;
            false_facts += self.cur_facts.false_facts.take();

            if ends_with_ret || last {
                true_facts = CondFacts::default();
                true_facts += false_facts.clone();
            }
        }

        if ends_with_ret {
            self.scope.facts.extend(false_facts);
        }

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn try_assign(&mut self, span: Span, op: AssignOp, lhs: &RPatOrExpr, ty: &Type) {
        let res: ValidationResult<()> = try {
            match *lhs {
                RPatOrExpr::Expr(ref expr) | RPatOrExpr::Pat(box RPat::Expr(ref expr)) => {
                    let lhs_ty = expr.validate_with_args(self, (TypeOfMode::LValue, None, None))?;
                    let lhs_ty = self.expand(span, lhs_ty)?;

                    if op == op!("=") {
                        self.assign(&lhs_ty, &ty, span)?;
                    } else {
                        self.assign_with_op(span, op, &lhs_ty, &ty)?;
                    }
                }

                RPatOrExpr::Pat(ref pat) => {
                    if op == op!("=") {
                        self.try_assign_pat(span, pat, ty)?;
                    } else {
                        // TODO
                        match &**pat {
                            RPat::Ident(left) => {
                                let lhs = self.type_of_var(left, TypeOfMode::LValue, None)?;
                                self.assign_with_op(span, op, &lhs, &ty)?;
                            }
                            _ => Err(Error::InvalidOperatorForLhs { span, op })?,
                        }
                    }
                }
            }
        };

        match res {
            Ok(()) => {}
            Err(err) => self.storage.report(err),
        }
    }

    fn try_assign_pat(&mut self, span: Span, lhs: &RPat, ty: &Type) -> ValidationResult<()> {
        match ty {
            Type::Ref(..) => {
                let ty = self
                    .expand_top_ref(span, Cow::Borrowed(ty))
                    .context("tried to expand reference to assign it to a pattern")?;

                return self
                    .try_assign_pat(span, lhs, &ty)
                    .context("tried to assign expanded type to a pattern");
            }
            _ => {}
        }

        // Update variable's type
        match lhs {
            // We emitted some parsing errors.
            RPat::Invalid(..) => return Ok(()),

            RPat::Assign(assign) => {
                let ids: Vec<Id> = find_ids_in_pat(&assign.left);

                self.try_assign_pat(span, &assign.left, ty)?;

                let prev_len = self.scope.declaring.len();
                self.scope.declaring.extend(ids);

                // TODO: Use type annotation?
                let res = assign
                    .right
                    .validate_with_default(self)
                    .context("tried to validate type of default expression in an assginment pattern");

                self.scope.declaring.drain(prev_len..);
                let default_value_type = res?;
                return self.try_assign_pat(span, &assign.left, &default_value_type);
            }

            RPat::Ident(i) => {
                // Verify using immutable references.
                if let Some(var_info) = self.scope.get_var(&i.into()) {
                    if let Some(var_ty) = var_info.ty.clone() {
                        self.assign(&var_ty, ty, i.span)?;
                    }
                }

                let mut actual_ty = None;
                if let Some(var_info) = self.scope.get_var(&i.into()) {
                    if let Some(declared_ty) = &var_info.ty {
                        if declared_ty.is_any() {
                            return Ok(());
                        }

                        actual_ty = Some(self.narrowed_type_of_assignment(declared_ty.clone(), &ty)?);
                    }
                }

                // TODO: Update actual types.
                if let Some(var_info) = self.scope.get_var_mut(&i.into()) {
                    var_info.actual_ty = Some(actual_ty.unwrap_or_else(|| ty.clone()));
                    return Ok(());
                }

                let var_info = if let Some(var_info) = self.scope.search_parent(&i.into()) {
                    let actual_ty = Some(actual_ty.unwrap_or_else(|| ty.clone()));

                    VarInfo {
                        actual_ty,
                        copied: true,
                        ..var_info.clone()
                    }
                } else {
                    if let Some(types) = self.find_type(self.ctx.module_id, &i.into())? {
                        for ty in types {
                            match &*ty {
                                Type::Module(..) => {
                                    return Err(Error::NotVariable {
                                        span: i.span,
                                        left: lhs.span(),
                                    });
                                }
                                _ => {}
                            }
                        }
                    }

                    return if self.ctx.allow_ref_declaring && self.scope.declaring.contains(&i.into()) {
                        Ok(())
                    } else {
                        // undefined symbol
                        Err(Error::UndefinedSymbol {
                            sym: i.into(),
                            span: i.span,
                        })
                    };
                };

                // Variable is defined on parent scope.
                //
                // We copy varinfo with enhanced type.
                self.scope.insert_var(i.into(), var_info);

                return Ok(());
            }

            RPat::Array(ref arr) => {
                //
                for (i, elem) in arr.elems.iter().enumerate() {
                    if let Some(elem) = elem {
                        match ty.normalize() {
                            ty if ty.is_any() => {
                                self.try_assign_pat(span, elem, ty)?;
                            }

                            Type::Tuple(Tuple { elems, .. }) => {
                                if elems.len() > i {
                                    self.try_assign_pat(span, elem, &elems[i].ty)?;
                                }
                            }

                            _ => unimplemented!("assignment with array pattern\nPat: {:?}\nType: {:?}", lhs, ty),
                        }
                    }
                }
                return Ok(());
            }

            RPat::Object(ref obj) => {
                //
                for prop in obj.props.iter() {
                    match ty.normalize() {
                        ty if ty.is_any() => {
                            let lhs = match prop {
                                RObjectPatProp::KeyValue(kv) => &kv.value,
                                RObjectPatProp::Assign(a) => {
                                    if a.key.type_ann.is_none() {
                                        if let Some(m) = &mut self.mutations {
                                            m.for_pats.entry(a.key.node_id).or_default().ty = Some(Type::any(span));
                                        }
                                    }
                                    continue;
                                }
                                RObjectPatProp::Rest(r) => {
                                    if r.type_ann.is_none() {
                                        if let Some(m) = &mut self.mutations {
                                            m.for_pats.entry(r.node_id).or_default().ty = Some(Type::any(span));
                                        }
                                    }
                                    continue;
                                }
                            };
                            self.try_assign_pat(span, lhs, &Type::any(ty.span()))?;
                        }

                        Type::TypeLit(TypeLit { span, ref members, .. }) => {
                            // Iterate over members, and assign if key matches.
                            for member in members {
                                match member {
                                    TypeElement::Call(_) => unimplemented!(),
                                    TypeElement::Constructor(_) => unimplemented!(),
                                    TypeElement::Property(p) => match prop {
                                        RObjectPatProp::KeyValue(prop) => {
                                            //
                                        }
                                        RObjectPatProp::Assign(_) => {}
                                        RObjectPatProp::Rest(_) => {}
                                    },
                                    TypeElement::Method(_) => unimplemented!(),
                                    TypeElement::Index(_) => unimplemented!(),
                                }
                            }
                        }
                        _ => unimplemented!("assignment with object pattern\nPat: {:?}\nType: {:?}", lhs, ty),
                    }
                }

                return Ok(());
            }

            RPat::Rest(rest) => {
                // TODO: Check if this is correct. (in object rest context)
                let ty = Type::Array(Array {
                    span,
                    elem_type: box ty.clone(),
                });
                return self.try_assign_pat(span, &rest.arg, &ty);
            }

            _ => {}
        }

        unimplemented!("assignment with complex pattern\nPat: {:?}\nType: {:?}", lhs, ty)
    }

    pub(super) fn add_type_fact(&mut self, sym: &Id, ty: Type) {
        slog::info!(self.logger, "add_type_fact({}); ty = {:?}", sym, ty);
        self.cur_facts.insert_var(sym, ty, false);
    }

    pub(super) fn add_deep_type_fact(&mut self, sym: Name, ty: Type, is_for_true: bool) {
        if is_for_true {
            self.cur_facts.true_facts.vars.insert(sym, ty);
        } else {
            self.cur_facts.false_facts.vars.insert(sym, ty);
        }
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, e: &RCondExpr, mode: TypeOfMode, type_ann: Option<&Type>) -> ValidationResult {
        self.record(e);

        let RCondExpr {
            span,
            ref test,
            ref alt,
            ref cons,
            ..
        } = *e;

        {
            let ctx = Ctx {
                in_cond: true,
                should_store_truthy_for_access: true,
                ..self.ctx
            };
            test.validate_with_default(&mut *self.with_ctx(ctx))?;
        }
        let true_facts = self.cur_facts.true_facts.take();
        let false_facts = self.cur_facts.false_facts.take();
        let cons = self.with_child(ScopeKind::Flow, true_facts, |child| {
            cons.validate_with_args(child, (mode, None, type_ann))
        })?;
        let alt = self.with_child(ScopeKind::Flow, false_facts, |child| {
            alt.validate_with_args(child, (mode, None, type_ann))
        })?;

        if cons.type_eq(&alt) {
            return Ok(cons);
        }

        let new_types = self.adjust_ternary_type(span, vec![cons, alt])?;
        Ok(Type::union(new_types))
    }
}

impl Facts {
    fn insert_var<N: Into<Name>>(&mut self, name: N, ty: Type, negate: bool) {
        let name = name.into();

        if negate {
            self.false_facts.vars.insert(name.clone(), ty.clone());
            self.true_facts.excludes.entry(name).or_default().push(ty);
        } else {
            self.true_facts.vars.insert(name.clone(), ty.clone());
            self.false_facts.excludes.entry(name).or_default().push(ty);
        }
    }
}
