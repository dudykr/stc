use crate::{
    analyzer::{
        assign::AssignOpts,
        expr::{IdCtx, TypeOfMode},
        marks::MarkExt,
        scope::{ScopeKind, VarInfo},
        util::ResultExt,
        Analyzer, Ctx,
    },
    ty::{Tuple, Type},
    type_facts::TypeFacts,
    util::{type_ext::TypeVecExt, EndsWithRet},
    validator,
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::FxHashMap;
use rnode::{NodeId, VisitWith};
use stc_ts_ast_rnode::{
    RBinExpr, RBindingIdent, RCondExpr, RExpr, RIdent, RIfStmt, RObjectPatProp, RPat, RPatOrExpr, RStmt, RSwitchCase,
    RSwitchStmt, RTsKeywordType,
};
use stc_ts_errors::{DebugExt, Error};
use stc_ts_type_ops::Fix;
use stc_ts_types::{name::Name, Array, Id, Key, Union};
use stc_ts_utils::MapWithMut;
use stc_utils::ext::SpanExt;
use std::{
    borrow::{Borrow, Cow},
    collections::hash_map::Entry,
    hash::Hash,
    mem::{replace, take},
    ops::{AddAssign, BitOr, Not},
};
use swc_atoms::JsWord;
use swc_common::{Span, Spanned, TypeEq, DUMMY_SP};
use swc_ecma_ast::*;

/// Conditional facts
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct CondFacts {
    pub facts: FxHashMap<Name, TypeFacts>,
    pub vars: FxHashMap<Name, Type>,
    pub excludes: FxHashMap<Name, Vec<Type>>,
    pub types: FxHashMap<Id, Type>,
}

impl CondFacts {
    #[inline]
    pub(crate) fn assert_valid(&self) {
        for (_, ty) in &self.vars {
            ty.assert_valid();
        }

        for (_, types) in &self.excludes {
            for ty in types {
                ty.assert_valid();
            }
        }

        for (_, ty) in &self.types {
            ty.assert_valid();
        }
    }

    #[inline]
    pub(crate) fn assert_clone_cheap(&self) {
        if !cfg!(debug_assertions) {
            return;
        }

        for (_, ty) in &self.vars {
            if !ty.is_union_type() {
                debug_assert!(
                    ty.is_clone_cheap(),
                    "ty.is_clone_cheap() shoulf be true:\n{:?}",
                    &self.vars
                );
            }
        }

        for (_, types) in &self.excludes {
            for ty in types {
                debug_assert!(ty.is_clone_cheap());
            }
        }

        for (_, ty) in &self.types {
            debug_assert!(ty.is_clone_cheap());
        }
    }

    pub fn override_vars_using(&mut self, r: &mut Self) {
        for (k, ty) in r.vars.drain() {
            match self.vars.entry(k) {
                Entry::Occupied(mut e) => {
                    *e.get_mut() = ty;
                }
                Entry::Vacant(e) => {
                    e.insert(ty);
                }
            }
        }
    }

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
    #[inline]
    pub(crate) fn assert_valid(&self) {
        self.true_facts.assert_valid();
        self.false_facts.assert_valid();
    }

    #[inline]
    pub(crate) fn assert_clone_cheap(&self) {
        self.true_facts.assert_clone_cheap();
        self.false_facts.assert_clone_cheap();
    }

    pub fn clear(&mut self) {
        self.assert_valid();

        self.true_facts.clear();
        self.false_facts.clear();
    }

    pub fn take(&mut self) -> Self {
        self.assert_valid();

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
        self.assert_valid();
        rhs.assert_valid();

        for (k, v) in rhs.facts {
            *self.facts.entry(k.clone()).or_insert(TypeFacts::None) |= v;
        }

        self.types.extend(rhs.types);

        for (k, v) in rhs.vars {
            match self.vars.entry(k) {
                Entry::Occupied(mut e) => match e.get_mut().normalize_mut() {
                    Type::Union(u) => {
                        u.types.push(v);
                    }
                    prev => {
                        let prev = prev.take();
                        *e.get_mut() = Type::union(vec![prev, v]).cheap();
                    }
                },
                Entry::Vacant(e) => {
                    e.insert(v);
                }
            }
        }

        for (k, v) in rhs.excludes {
            self.excludes.entry(k).or_default().extend(v);
        }
    }
}

impl AddAssign<Option<Self>> for CondFacts {
    fn add_assign(&mut self, rhs: Option<Self>) {
        self.assert_valid();

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
        let prev_facts = self.cur_facts.take();
        prev_facts.assert_clone_cheap();

        let facts_from_test: Facts = {
            let ctx = Ctx {
                in_cond: true,
                should_store_truthy_for_access: true,
                ..self.ctx
            };
            let facts = self.with_ctx(ctx).with_child(
                ScopeKind::Flow,
                prev_facts.true_facts.clone(),
                |child: &mut Analyzer| {
                    let test = stmt.test.validate_with_default(child);
                    match test {
                        Ok(_) => {}
                        Err(err) => {
                            child.storage.report(err);
                        }
                    }

                    Ok(child.cur_facts.take())
                },
            );

            facts.report(&mut self.storage).unwrap_or_default()
        };

        facts_from_test.assert_clone_cheap();

        let true_facts = facts_from_test.true_facts;
        let false_facts = facts_from_test.false_facts;

        let mut cons_ends_with_unreachable = false;

        let ends_with_ret = stmt.cons.ends_with_ret();

        self.cur_facts = prev_facts.clone();
        self.with_child(ScopeKind::Flow, true_facts, |child: &mut Analyzer| {
            stmt.cons.visit_with(child);

            cons_ends_with_unreachable = child.ctx.in_unreachable;

            Ok(())
        })
        .report(&mut self.storage);

        let mut alt_ends_with_unreachable = None;

        if let Some(alt) = &stmt.alt {
            self.cur_facts = prev_facts.clone();
            self.with_child(ScopeKind::Flow, false_facts.clone(), |child: &mut Analyzer| {
                alt.visit_with(child);

                alt_ends_with_unreachable = Some(child.ctx.in_unreachable);

                Ok(())
            })
            .report(&mut self.storage);
        }

        self.cur_facts = prev_facts;

        if ends_with_ret {
            self.cur_facts.true_facts += false_facts;
        }

        if cons_ends_with_unreachable {
            if let Some(true) = alt_ends_with_unreachable {
                self.ctx.in_unreachable = true;
            }
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

                    match self.extends(span, Default::default(), &b, ty) {
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

                match self.extends(span, Default::default(), &ty, b) {
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
        let mut base_true_facts = self.cur_facts.true_facts.take();
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

            let true_facts_created_by_case = self.cur_facts.true_facts.take();
            let false_facts_created_by_case = self.cur_facts.false_facts.take();

            let mut facts_for_body = base_true_facts.clone();
            facts_for_body += true_facts_created_by_case;

            self.with_child(ScopeKind::Flow, facts_for_body, |child| {
                cons.visit_with(child);
                Ok(())
            })?;

            if ends_with_ret || last {
                false_facts += false_facts_created_by_case.clone();
                base_true_facts += false_facts_created_by_case;
            }
        }

        if !errored {
            self.ctx.in_unreachable |= stmt
                .cases
                .iter()
                .all(|case| self.is_switch_case_body_unconditional_termination(&case.cons));
        }

        if ends_with_ret {
            self.cur_facts.true_facts += false_facts;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct PatAssignOpts {
    pub assign: AssignOpts,
    pub ignore_lhs_errors: bool,
}

impl Analyzer<'_, '_> {
    /// Returns true if a body of switch always ends with `return`, `throw` or
    /// `continue`.
    ///
    /// TODO: Support break with other label.
    fn is_switch_case_body_unconditional_termination<S>(&mut self, body: &[S]) -> bool
    where
        S: Borrow<RStmt>,
    {
        for stmt in body {
            match stmt.borrow() {
                RStmt::Return(..) | RStmt::Throw(..) | RStmt::Continue(..) => return true,
                RStmt::Break(..) => return false,

                RStmt::If(s) => match &s.alt {
                    Some(alt) => {
                        return self.is_switch_case_body_unconditional_termination(&[&*s.cons])
                            && self.is_switch_case_body_unconditional_termination(&[&**alt]);
                    }
                    None => return self.is_switch_case_body_unconditional_termination(&[&*s.cons]),
                },
                _ => {}
            }
        }

        false
    }

    pub(super) fn try_assign(&mut self, span: Span, op: AssignOp, lhs: &RPatOrExpr, ty: &Type) {
        ty.assert_valid();

        let res: ValidationResult<()> = try {
            match *lhs {
                RPatOrExpr::Expr(ref expr) | RPatOrExpr::Pat(box RPat::Expr(ref expr)) => {
                    let lhs_ty = expr.validate_with_args(self, (TypeOfMode::LValue, None, None));
                    let lhs_ty = match lhs_ty {
                        Ok(v) => v,
                        _ => return,
                    };

                    if op == op!("=") {
                        self.assign(&mut Default::default(), &lhs_ty, &ty, span)?;
                    } else {
                        self.assign_with_op(span, op, &lhs_ty, &ty)?;
                    }
                }

                RPatOrExpr::Pat(ref pat) => {
                    if op == op!("=") {
                        self.try_assign_pat_with_opts(
                            span,
                            pat,
                            ty,
                            PatAssignOpts {
                                ignore_lhs_errors: true,
                                ..Default::default()
                            },
                        )?;
                    } else {
                        // TODO
                        match &**pat {
                            RPat::Ident(left) => {
                                let lhs = self.type_of_var(&left.id, TypeOfMode::LValue, None);

                                if let Ok(lhs) = lhs {
                                    self.assign_with_op(span, op, &lhs, &ty)?;
                                }
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

    pub(super) fn try_assign_pat(&mut self, span: Span, lhs: &RPat, ty: &Type) -> ValidationResult<()> {
        ty.assert_valid();

        self.try_assign_pat_with_opts(span, lhs, ty, Default::default())
    }

    fn try_assign_pat_with_opts(
        &mut self,
        span: Span,
        lhs: &RPat,
        ty: &Type,
        opts: PatAssignOpts,
    ) -> ValidationResult<()> {
        ty.assert_valid();

        let is_in_loop = self.scope.is_in_loop_body();
        let ty = self
            .normalize(Some(ty.span().or_else(|| span)), Cow::Borrowed(ty), Default::default())
            .context("tried to normalize a type to assign it to a pattern")?;
        let ty = ty.normalize();

        ty.assert_valid();

        // Update variable's type
        match lhs {
            // We emitted some parsing errors.
            RPat::Invalid(..) => return Ok(()),

            RPat::Assign(assign) => {
                self.try_assign_pat_with_opts(span, &assign.left, &ty, opts)
                    .report(&mut self.storage);

                // TODO: Use type annotation?
                let res = assign
                    .right
                    .validate_with_default(self)
                    .context("tried to validate type of default expression in an assginment pattern");

                res.and_then(|default_value_type| {
                    self.try_assign_pat_with_opts(span, &assign.left, &default_value_type, opts)
                })
                .report(&mut self.storage);

                self.try_assign_pat_with_opts(span, &assign.left, ty, opts)?;
                return Ok(());
            }

            RPat::Ident(i) => {
                // Verify using immutable references.
                if let Some(var_info) = self.scope.get_var(&i.id.clone().into()) {
                    if let Some(var_ty) = var_info.ty.clone() {
                        self.assign_with_opts(
                            &mut Default::default(),
                            AssignOpts {
                                span: i.id.span,
                                ..opts.assign
                            },
                            &var_ty,
                            &ty,
                        )?;
                    }
                }

                let mut actual_ty = None;
                if let Some(var_info) = self
                    .scope
                    .get_var(&i.id.clone().into())
                    .or_else(|| self.scope.search_parent(&i.id.clone().into()))
                {
                    if let Some(declared_ty) = &var_info.ty {
                        declared_ty.assert_valid();

                        if declared_ty.is_any()
                            || ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                            || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                        {
                            return Ok(());
                        }

                        let declared_ty = declared_ty.clone();

                        let ty = ty.clone();
                        let ty = self.apply_type_facts_to_type(TypeFacts::NEUndefined | TypeFacts::NENull, ty);

                        ty.assert_valid();

                        if ty.is_never() {
                            return Ok(());
                        }

                        let narrowed_ty = self.narrowed_type_of_assignment(span, declared_ty, &ty)?;
                        narrowed_ty.assert_valid();
                        actual_ty = Some(narrowed_ty);
                    }
                } else {
                    if !opts.ignore_lhs_errors {
                        self.storage.report(Error::NoSuchVar {
                            span,
                            name: i.id.clone().into(),
                        });
                    }
                    return Ok(());
                }

                if let Some(ty) = &actual_ty {
                    ty.assert_valid();
                }

                // Update actual types.
                if let Some(var_info) = self.scope.get_var_mut(&i.id.clone().into()) {
                    var_info.is_actual_type_modified_in_loop |= is_in_loop;
                    let new_ty = actual_ty.unwrap_or_else(|| ty.clone());
                    new_ty.assert_valid();
                    var_info.actual_ty = Some(new_ty);
                    return Ok(());
                }

                let var_info = if let Some(var_info) = self.scope.search_parent(&i.id.clone().into()) {
                    let actual_ty = actual_ty.unwrap_or_else(|| ty.clone());
                    actual_ty.assert_valid();

                    VarInfo {
                        actual_ty: Some(actual_ty),
                        copied: true,
                        ..var_info.clone()
                    }
                } else {
                    if let Some(types) = self.find_type(self.ctx.module_id, &i.id.clone().into())? {
                        for ty in types {
                            match &*ty {
                                Type::Module(..) => {
                                    return Err(Error::NotVariable {
                                        span: i.id.span,
                                        left: lhs.span(),
                                    });
                                }
                                _ => {}
                            }
                        }
                    }

                    return if self.ctx.allow_ref_declaring && self.scope.declaring.contains(&i.id.clone().into()) {
                        Ok(())
                    } else {
                        // undefined symbol
                        Err(Error::UndefinedSymbol {
                            sym: i.id.clone().into(),
                            span: i.id.span,
                        })
                    };
                };

                // Variable is defined on parent scope.
                //
                // We copy varinfo with enhanced type.
                self.scope.insert_var(i.id.clone().into(), var_info);

                return Ok(());
            }

            RPat::Array(ref arr) => {
                let ty = self
                    .get_iterator(span, Cow::Borrowed(&ty), Default::default())
                    .context("tried to convert a type to an iterator to assign with an array pattern")
                    .report(&mut self.storage)
                    .unwrap_or_else(|| Cow::Owned(Type::any(span)));
                //
                for (i, elem) in arr.elems.iter().enumerate() {
                    if let Some(elem) = elem {
                        match elem {
                            RPat::Rest(elem) => {
                                // Rest element is special.
                                let type_for_rest_arg = self.get_lefting_elements(None, ty, i).context(
                                    "tried to get lefting elements of an iterator to assign using a rest pattern",
                                )?;

                                self.try_assign_pat_with_opts(
                                    span,
                                    &elem.arg,
                                    &type_for_rest_arg,
                                    PatAssignOpts {
                                        assign: AssignOpts {
                                            allow_iterable_on_rhs: true,
                                            ..opts.assign
                                        },
                                        ..opts
                                    },
                                )
                                .context("tried to assign lefting elements to the arugment of a rest pattern")?;
                                break;
                            }
                            _ => {}
                        }

                        match ty.normalize() {
                            Type::Tuple(Tuple { elems, .. }) => {
                                if elems.len() > i {
                                    self.try_assign_pat_with_opts(span, elem, &elems[i].ty, opts)
                                        .report(&mut self.storage);
                                } else {
                                    self.storage.report(Error::TupleIndexError {
                                        span,
                                        len: elems.len() as _,
                                        index: i as _,
                                    });
                                }
                            }

                            _ => {
                                let elem_ty = self
                                    .get_element_from_iterator(span, Cow::Borrowed(&ty), i)
                                    .context("tried to get an element of type to assign with an array pattern")
                                    .report(&mut self.storage);
                                if let Some(elem_ty) = elem_ty {
                                    self.try_assign_pat_with_opts(span, elem, &elem_ty, opts)
                                        .context("tried to assign an element of an array pattern")
                                        .report(&mut self.storage);
                                }
                            }
                        }
                    }
                }
                return Ok(());
            }

            RPat::Object(ref obj) => {
                //
                for prop in obj.props.iter() {
                    match prop {
                        RObjectPatProp::KeyValue(kv) => {
                            let key = kv.key.validate_with(self)?;
                            let ctx = Ctx {
                                disallow_indexing_array_with_string: true,
                                ..self.ctx
                            };
                            let prop_ty = self
                                .with_ctx(ctx)
                                .access_property(span, ty, &key, TypeOfMode::RValue, IdCtx::Var)
                                .unwrap_or_else(|_| Type::any(span));

                            self.try_assign_pat_with_opts(span, &kv.value, &prop_ty, opts)
                                .report(&mut self.storage);
                        }
                        RObjectPatProp::Assign(a) => {
                            let key = Key::Normal {
                                span: a.key.span,
                                sym: a.key.sym.clone(),
                            };
                            let ctx = Ctx {
                                disallow_indexing_array_with_string: true,
                                ..self.ctx
                            };
                            let prop_ty = self
                                .with_ctx(ctx)
                                .access_property(span, ty, &key, TypeOfMode::RValue, IdCtx::Var)
                                .unwrap_or_else(|_| Type::any(span));

                            self.try_assign_pat_with_opts(
                                span,
                                &RPat::Ident(RBindingIdent {
                                    node_id: NodeId::invalid(),
                                    id: a.key.clone(),
                                    type_ann: None,
                                }),
                                &prop_ty,
                                opts,
                            )
                            .report(&mut self.storage);
                        }
                        RObjectPatProp::Rest(r) => {
                            if r.type_ann.is_none() {
                                if let Some(m) = &mut self.mutations {
                                    m.for_pats.entry(r.node_id).or_default().ty = Some(Type::any(span));
                                }
                            }

                            match &*r.arg {
                                RPat::Ident(_) => {}

                                RPat::Array(_) => {
                                    self.storage.report(Error::NotArrayType { span: r.arg.span() });
                                    self.storage
                                        .report(Error::BindingPatNotAllowedInRestPatArg { span: r.arg.span() });
                                }

                                RPat::Object(_) => {
                                    self.storage
                                        .report(Error::BindingPatNotAllowedInRestPatArg { span: r.arg.span() });
                                }

                                RPat::Expr(_) => {
                                    self.storage
                                        .report(Error::BindingPatNotAllowedInRestPatArg { span: r.arg.span() });
                                }

                                RPat::Invalid(_) => {
                                    self.storage
                                        .report(Error::BindingPatNotAllowedInRestPatArg { span: r.arg.span() });
                                    self.storage
                                        .report(Error::RestArgMustBeVarOrMemberAccess { span: r.arg.span() });
                                }

                                _ => {}
                            }
                            // TODO
                            // self.try_assign_pat_with_opts(span, lhs,
                            // &prop_ty).report(&mut self.storage);
                        }
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
                return self.try_assign_pat_with_opts(span, &rest.arg, &ty, opts);
            }

            RPat::Expr(lhs) => {
                match &**lhs {
                    RExpr::Lit(..) => {
                        self.storage.report(Error::InvalidLhsOfAssign { span: lhs.span() });
                        return Ok(());
                    }
                    _ => {}
                }
                let lhs_ty = lhs
                    .validate_with_args(self, (TypeOfMode::LValue, None, None))
                    .context("tried to validate type of the expression in lhs of assignment")
                    .report(&mut self.storage);

                if let Some(lhs_ty) = &lhs_ty {
                    self.assign_with_opts(
                        &mut Default::default(),
                        AssignOpts { span, ..opts.assign },
                        &lhs_ty,
                        &ty,
                    )?;
                }
                return Ok(());
            }
        }
    }

    pub(super) fn add_type_fact(&mut self, sym: &Id, ty: Type) {
        slog::info!(self.logger, "add_type_fact({}); ty = {:?}", sym, ty);

        debug_assert!(ty.is_clone_cheap());

        ty.assert_valid();

        self.cur_facts.insert_var(sym, ty, false);
    }

    pub(super) fn add_deep_type_fact(&mut self, span: Span, name: Name, ty: Type, is_for_true: bool) {
        debug_assert!(!self.is_builtin);

        debug_assert!(ty.is_clone_cheap());

        ty.assert_valid();

        if let Some((name, mut ty)) = self
            .determine_type_fact_by_field_fact(span, &name, &ty)
            .report(&mut self.storage)
            .flatten()
        {
            ty.make_cheap();
            ty.assert_valid();

            if is_for_true {
                self.cur_facts.true_facts.vars.insert(name, ty);
            } else {
                self.cur_facts.false_facts.vars.insert(name, ty);
            }
            return;
        }

        if is_for_true {
            self.cur_facts.true_facts.vars.insert(name, ty);
        } else {
            self.cur_facts.false_facts.vars.insert(name, ty);
        }
    }

    /// If `type_facts` is [None], this method calculates type facts created by
    /// `'foo' in obj`.
    ///
    /// Otherwise, this method calculates type facts created by `if (a.foo) ;`.
    /// In this case, this method tests if `type_facts` matches the type of
    /// property and returns `never` if it does not.
    pub(super) fn filter_types_with_property(
        &mut self,
        src: &Type,
        property: &JsWord,
        type_facts: Option<TypeFacts>,
    ) -> ValidationResult<Type> {
        src.assert_valid();

        match src.normalize() {
            Type::Ref(..) => {
                let src = self.expand_top_ref(src.span(), Cow::Borrowed(src))?;
                return self.filter_types_with_property(&src, property, type_facts);
            }
            Type::Union(ty) => {
                let mut new_types = vec![];
                for ty in &ty.types {
                    let ty = self.filter_types_with_property(&ty, property, type_facts)?;
                    new_types.push(ty);
                }
                new_types.retain(|ty| !ty.is_never());
                new_types.dedup_type();

                if new_types.len() == 1 {
                    return Ok(new_types.into_iter().next().unwrap());
                }

                return Ok(Type::Union(Union {
                    span: ty.span(),
                    types: new_types,
                }));
            }
            _ => {}
        }

        let ctx = Ctx {
            disallow_creating_indexed_type_from_ty_els: true,
            ..self.ctx
        };
        let prop_res = self.with_ctx(ctx).access_property(
            src.span(),
            src,
            &Key::Normal {
                span: DUMMY_SP,
                sym: property.clone(),
            },
            TypeOfMode::RValue,
            IdCtx::Var,
        );

        match prop_res {
            Ok(mut prop_ty) => {
                // Check if property matches the type fact.
                if let Some(type_facts) = type_facts {
                    let orig = prop_ty.clone();
                    prop_ty = self.apply_type_facts_to_type(type_facts, prop_ty);

                    // TODO: See if which one is correct.
                    //
                    // if !orig.normalize().type_eq(prop_ty.normalize()) {
                    //     return Ok(Type::never(src.span()));
                    // }

                    if prop_ty.is_never() {
                        return Ok(Type::never(src.span()));
                    }
                }
            }
            Err(err) => match err.actual() {
                Error::NoSuchProperty { .. } | Error::NoSuchPropertyInClass { .. } => {
                    return Ok(Type::never(src.span()))
                }
                _ => {}
            },
        }

        Ok(src.clone())
    }

    fn determine_type_fact_by_field_fact(
        &mut self,
        span: Span,
        name: &Name,
        ty: &Type,
    ) -> ValidationResult<Option<(Name, Type)>> {
        ty.assert_valid();

        if name.len() == 1 {
            return Ok(None);
        }

        let ids = name.as_ids();
        let mut id: RIdent = ids[0].clone().into();
        id.span.lo = span.lo;
        id.span.hi = span.hi;

        let obj = self.type_of_var(&id, TypeOfMode::RValue, None)?;
        let obj = self.expand_top_ref(ty.span(), Cow::Owned(obj))?;

        match obj.normalize() {
            Type::Union(u) => {
                if ids.len() == 2 {
                    let mut new_obj_types = vec![];

                    for obj in &u.types {
                        if let Ok(prop_ty) = self.access_property(
                            obj.span(),
                            obj,
                            &Key::Normal {
                                span: ty.span(),
                                sym: ids[1].sym().clone(),
                            },
                            TypeOfMode::RValue,
                            IdCtx::Var,
                        ) {
                            if ty.type_eq(&prop_ty) {
                                new_obj_types.push(obj.clone());
                            }
                        }
                    }

                    if new_obj_types.is_empty() {
                        return Ok(None);
                    }
                    let mut ty = Type::union(new_obj_types);
                    ty.fix();

                    return Ok(Some((Name::from(ids[0].clone()), ty)));
                }
            }
            _ => {}
        }

        Ok(None)
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

        self.validate_with(|a| {
            let ctx = Ctx {
                in_cond: true,
                should_store_truthy_for_access: true,
                ..a.ctx
            };
            test.validate_with_default(&mut *a.with_ctx(ctx))?;

            Ok(())
        });

        let true_facts = self.cur_facts.true_facts.take();
        let false_facts = self.cur_facts.false_facts.take();
        let cons = self.with_child(ScopeKind::Flow, true_facts, |child: &mut Analyzer| {
            let ty = cons
                .validate_with_args(child, (mode, None, type_ann))
                .report(&mut child.storage);

            Ok(ty.unwrap_or_else(|| Type::any(cons.span())))
        })?;
        let alt = self.with_child(ScopeKind::Flow, false_facts, |child: &mut Analyzer| {
            let ty = alt
                .validate_with_args(child, (mode, None, type_ann))
                .report(&mut child.storage);

            Ok(ty.unwrap_or_else(|| Type::any(alt.span())))
        })?;

        if cons.type_eq(&alt) {
            return Ok(cons);
        }

        let new_types = if type_ann.is_none() {
            self.adjust_ternary_type(span, vec![cons, alt])?
        } else {
            vec![cons, alt]
        };
        let mut ty = Type::union(new_types).fixed();
        ty.reposition(span);
        ty.assert_valid();
        Ok(ty)
    }
}

impl Facts {
    fn insert_var<N: Into<Name>>(&mut self, name: N, ty: Type, negate: bool) {
        ty.assert_valid();

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
