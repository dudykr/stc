pub(crate) use self::vars::VarKind;
use super::assign::AssignOpts;
use super::class::ClassState;
use super::{control_flow::CondFacts, expr::TypeOfMode, stmt::return_type::ReturnValues, Analyzer, Ctx};
use crate::analyzer::expr::IdCtx;
use crate::analyzer::scope::vars::DeclareVarsOpts;
use crate::analyzer::ResultExt;
use crate::{
    loader::ModuleInfo,
    ty::{self, Alias, Interface, PropertySignature, Ref, Tuple, Type, TypeExt, TypeLit, Union},
    type_facts::TypeFacts,
    util::{contains_infer_type, contains_mark, MarkFinder},
    ValidationResult,
};
use fxhash::{FxHashMap, FxHashSet};
use iter::once;
use once_cell::sync::Lazy;
use rnode::Fold;
use rnode::FoldWith;
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use slog::Logger;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsQualifiedName;
use stc_ts_errors::debug::dump_type_as_string;
use stc_ts_errors::debug::print_backtrace;
use stc_ts_errors::DebugExt;
use stc_ts_errors::Error;
use stc_ts_type_ops::Fix;
use stc_ts_types::name::Name;
use stc_ts_types::Class;
use stc_ts_types::ClassDef;
use stc_ts_types::ClassProperty;
use stc_ts_types::EnumVariant;
use stc_ts_types::Intersection;
use stc_ts_types::Key;
use stc_ts_types::TypeElement;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::{
    Conditional, FnParam, Id, IndexedAccessType, Mapped, ModuleId, Operator, QueryExpr, QueryType, StaticThis,
    TypeParam,
};
use stc_utils::error::context;
use stc_utils::stack;
use std::mem::replace;
use std::mem::take;
use std::{borrow::Cow, collections::hash_map::Entry, fmt::Debug, iter, slice};
use swc_atoms::js_word;
use swc_common::Spanned;
use swc_common::TypeEq;
use swc_common::{util::move_map::MoveMap, Mark, Span, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::*;

mod this;
mod type_param;
mod vars;

macro_rules! no_ref {
    ($t:expr) => {{
        match $t {
            Some(Type::Ref(..)) => panic!("cannot store a variable with type `Ref`"),
            _ => {}
        }
    }};
}

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    logger: Logger,
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    pub declaring: Vec<Id>,

    pub declared_return_type: Option<Type>,

    pub declaring_type_params: FxHashSet<Id>,

    pub(super) vars: FxHashMap<Id, VarInfo>,
    types: FxHashMap<Id, Type>,
    pub(super) facts: CondFacts,

    pub(super) declaring_fn: Option<Id>,
    /// [Some] while declaring a class property or a property of an object
    /// literal.
    pub(super) declaring_prop: Option<Id>,

    pub(super) this: Option<Type>,

    /// Used while validating super class and static class properties. Otherwise
    /// [None].
    ///
    /// Required to handle static properies.
    pub(super) this_class_name: Option<Id>,
    /// Only contains instance members.
    ///
    /// The value of [usize] should be ignored by methods except
    /// `Validate<Class>`
    pub(super) this_class_members: Vec<(usize, ty::ClassMember)>,

    pub(super) this_object_members: Vec<TypeElement>,

    pub(super) super_class: Option<Type>,

    pub(super) return_values: ReturnValues,

    /// `0` if we are not trying to expand it.
    expand_triage_depth: u8,

    /// Used to handle `...any` in calls.
    pub(super) is_call_arg_count_unknown: bool,

    pub(super) type_params: FxHashMap<Id, Type>,

    /// If two modules have same name, the latter can reference exported members
    /// from other modules.
    ///
    /// It means we need a way to know which module we are in, and this field is
    /// used to store module name.
    pub(super) cur_module_name: Option<Id>,

    /// All states related to validation of a class.
    pub(super) class: ClassState,
}

impl Scope<'_> {
    pub fn parent(&self) -> Option<&Self> {
        self.parent
    }

    pub fn first<F>(&self, mut filter: F) -> Option<&Self>
    where
        F: FnMut(&Scope) -> bool,
    {
        if filter(&self) {
            return Some(self);
        }

        self.parent?.first(filter)
    }

    pub fn first_kind<F>(&self, mut filter: F) -> Option<&Self>
    where
        F: FnMut(ScopeKind) -> bool,
    {
        self.first(|scope| filter(scope.kind))
    }

    /// If `filter` returns [Some], this method returns it.
    pub fn matches<F>(&self, mut filter: F) -> Option<bool>
    where
        F: FnMut(&Self) -> Option<bool>,
    {
        let res = filter(self);
        match res {
            Some(v) => return Some(v),
            None => {}
        }

        self.parent?.matches(filter)
    }

    /// If `filter` returns [Some], this method returns it.
    pub fn matches_kind<F>(&self, mut filter: F) -> Option<bool>
    where
        F: FnMut(ScopeKind) -> Option<bool>,
    {
        self.matches(|scope| filter(scope.kind))
    }

    pub fn is_arguments_implicitly_defined(&self) -> bool {
        self.first(|scope| {
            if scope.is_root() {
                return false;
            }

            match scope.kind {
                ScopeKind::Fn | ScopeKind::Method { .. } => true,
                _ => false,
            }
        })
        .is_some()
    }

    pub fn is_declaring(&self, id: &Id) -> bool {
        if self.declaring.contains(id) {
            return true;
        }

        match self.parent {
            Some(s) => s.is_declaring(id),
            None => false,
        }
    }

    /// Get scope of computed property names.

    pub fn scope_of_computed_props(&self) -> Option<&Self> {
        self.scope_of_computed_props_inner()?.parent()
    }

    fn scope_of_computed_props_inner(&self) -> Option<&Self> {
        match self.kind {
            ScopeKind::Fn
            | ScopeKind::Method { .. }
            | ScopeKind::ArrowFn
            | ScopeKind::Class
            | ScopeKind::ObjectLit
            | ScopeKind::Module
            | ScopeKind::Call
            | ScopeKind::Constructor => Some(self),
            ScopeKind::LoopBody { .. } => self.parent()?.scope_of_computed_props(),
            ScopeKind::Block | ScopeKind::Flow | ScopeKind::TypeParams => self.parent()?.scope_of_computed_props(),
        }
    }

    pub fn get_type_facts(&self, name: &Name) -> TypeFacts {
        if let Some(&f) = self.facts.facts.get(name) {
            return f;
        }

        match self.parent {
            Some(parent) => parent.get_type_facts(name),
            _ => TypeFacts::None,
        }
    }

    pub fn is_declaring_fn(&self, id: &Id) -> bool {
        if let Some(d) = &self.declaring_fn {
            if *d == *id {
                return true;
            }
        }

        match self.parent {
            Some(scope) => scope.is_declaring_fn(id),
            _ => false,
        }
    }

    pub fn get_this_class_name(&self) -> Option<Id> {
        match &self.this_class_name {
            Some(v) => return Some(v.clone()),
            None => {}
        }

        self.parent.and_then(|parent| parent.get_this_class_name())
    }

    pub fn declaring_prop(&self) -> Option<Id> {
        if self.declaring_prop.is_some() {
            return self.declaring_prop.clone();
        }

        match self.parent {
            Some(parent) => parent.declaring_prop(),
            _ => None,
        }
    }

    pub(crate) fn is_this_defined(&self) -> bool {
        if self.is_root() {
            return false;
        }

        match self.kind {
            ScopeKind::Fn | ScopeKind::Method { .. } | ScopeKind::Class | ScopeKind::ObjectLit => return true,
            _ => {}
        }

        self.parent.map(|scope| scope.is_this_defined()).unwrap_or(false)
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }
    pub fn is_module(&self) -> bool {
        self.kind == ScopeKind::Module
    }

    /// Returns `true` if a scope exists for storing
    pub fn should_store_type_params(&self) -> bool {
        match self.kind {
            ScopeKind::Call | ScopeKind::TypeParams => return true,
            _ => {}
        }

        match self.parent {
            Some(v) => v.should_store_type_params(),
            None => false,
        }
    }

    pub fn is_in_call(&self) -> bool {
        match self.kind {
            ScopeKind::Call => return true,
            _ => {}
        }

        match self.parent {
            Some(v) => v.is_in_call(),
            None => false,
        }
    }

    pub fn is_in_loop_body(&self) -> bool {
        match self.kind {
            ScopeKind::LoopBody { .. } => return true,
            ScopeKind::Module | ScopeKind::ArrowFn | ScopeKind::Fn | ScopeKind::Class => return false,
            _ => {}
        }

        match self.parent {
            Some(v) => v.is_in_loop_body(),
            None => false,
        }
    }

    pub fn store_type_param(&mut self, name: Id, ty: Type) {
        self.type_params.insert(name, ty);
    }

    /// Get members of current class.
    pub fn class_members(&self) -> &[(usize, ty::ClassMember)] {
        if let ScopeKind::Class = self.kind {
            return &self.this_class_members;
        }

        match self.parent {
            Some(parent) => parent.class_members(),
            None => &[],
        }
    }

    /// Get members of the current object literal.
    pub fn object_lit_members(&self) -> &[TypeElement] {
        if let ScopeKind::ObjectLit = self.kind {
            return &self.this_object_members;
        }

        match self.parent {
            Some(parent) => parent.object_lit_members(),
            None => &[],
        }
    }

    pub fn get_super_class(&self) -> Option<&Type> {
        if let ScopeKind::Class = self.kind {
            return self.super_class.as_ref();
        }

        self.parent?.get_super_class()
    }

    pub fn remove_parent(self) -> Scope<'static> {
        Scope {
            logger: self.logger,
            parent: None,
            kind: self.kind,
            declaring: self.declaring,
            declared_return_type: self.declared_return_type,
            declaring_type_params: self.declaring_type_params,
            vars: self.vars,
            types: self.types,
            facts: self.facts,
            declaring_fn: self.declaring_fn,
            declaring_prop: self.declaring_prop,
            this: self.this,
            this_class_name: self.this_class_name,
            this_class_members: self.this_class_members,
            this_object_members: self.this_object_members,
            super_class: self.super_class,
            return_values: self.return_values,
            expand_triage_depth: self.expand_triage_depth,
            is_call_arg_count_unknown: self.is_call_arg_count_unknown,
            type_params: self.type_params,
            cur_module_name: self.cur_module_name,
            class: self.class,
        }
    }

    pub fn current_module_name(&self) -> Option<Id> {
        match &self.cur_module_name {
            Some(v) => return Some(v.clone()),
            _ => {}
        }

        self.parent?.current_module_name()
    }

    pub fn move_types_from_child(&mut self, child: &mut Scope) {
        for (name, ty) in child.types.drain() {
            if ty.normalize().is_type_param() {
                self.register_type(name, ty, false);
            }
        }
    }

    pub fn move_vars_from_child(&mut self, child: &mut Scope) {
        match child.kind {
            // We don't copy variable information from nested function.
            ScopeKind::Module | ScopeKind::Method { .. } | ScopeKind::Fn | ScopeKind::ArrowFn => return,
            _ => {}
        }
        let is_end_of_loop = match child.kind {
            ScopeKind::LoopBody { last: true } => true,
            _ => false,
        };

        for (name, var) in child.vars.drain() {
            if let Some(ty) = &var.ty {
                ty.assert_valid();
            }

            if let Some(ty) = &var.actual_ty {
                ty.assert_valid();
            }

            if var.copied {
                match self.vars.entry(name.clone()) {
                    Entry::Occupied(mut e) => {
                        e.get_mut().is_actual_type_modified_in_loop |= var.is_actual_type_modified_in_loop;
                        let is_actual_type_modified_in_loop = e.get().is_actual_type_modified_in_loop;

                        if let Some(actual_ty) = var.actual_ty {
                            actual_ty.assert_valid();

                            let new_actual_type = if is_end_of_loop && is_actual_type_modified_in_loop {
                                let mut types = vec![];

                                if let Some(prev) = &e.get().actual_ty {
                                    if !actual_ty.type_eq(prev) {
                                        types.push(actual_ty);
                                    }
                                } else {
                                    types.push(actual_ty);
                                }

                                types.extend(e.get().actual_ty.clone());

                                if types.len() == 1 {
                                    types.into_iter().next().unwrap().fixed()
                                } else {
                                    Type::Union(Union { span: DUMMY_SP, types }).fixed()
                                }
                            } else {
                                actual_ty
                            };

                            new_actual_type.assert_valid();

                            e.get_mut().actual_ty = Some(new_actual_type);
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(var);
                    }
                }
            } else if let VarKind::Decl(VarDeclKind::Var) | VarKind::Fn = var.kind {
                self.vars.insert(name, var);
            }
        }
    }

    pub fn declared_return_type(&self) -> Option<&Type> {
        match &self.declared_return_type {
            Some(v) => return Some(v),
            None => {}
        }
        match self.kind {
            ScopeKind::Fn | ScopeKind::Method { .. } | ScopeKind::Constructor | ScopeKind::ArrowFn => return None,
            _ => {}
        }

        self.parent?.declared_return_type()
    }

    pub fn remove_declaring<I>(&mut self, names: impl IntoIterator<IntoIter = I, Item = Id>)
    where
        I: Iterator<Item = Id> + DoubleEndedIterator,
    {
        for n in names.into_iter().rev() {
            let idx = self
                .declaring
                .iter()
                .rposition(|name| n == *name)
                .expect("failed to find inserted name");
            self.declaring.remove(idx);
        }
    }

    pub fn insert_var(&mut self, name: Id, v: VarInfo) {
        no_ref!(v.ty);

        if let Some(v) = &v.ty {
            v.assert_valid();
        }

        if let Some(v) = &v.actual_ty {
            v.assert_valid();
        }

        self.vars.insert(name, v);
    }

    /// This method does **not** search for parent scope.
    pub fn get_var_mut(&mut self, name: &Id) -> Option<&mut VarInfo> {
        self.vars.get_mut(name)
    }

    /// Add a type to the scope.
    fn register_type(&mut self, name: Id, ty: Type, should_override: bool) {
        ty.assert_valid();

        let ty = ty.cheap();
        match ty.normalize() {
            Type::Param(..) => {
                // Override type parameter.

                match self.types.entry(name) {
                    Entry::Occupied(mut e) => {
                        let prev = e.get_mut();

                        if prev.normalize().is_type_param() {
                            *prev = ty;
                            return;
                        } else if prev.normalize().is_intersection_type() {
                            match prev.normalize_mut() {
                                Type::Intersection(prev) => {
                                    if let Some(index) = prev.types.iter().position(|v| match v.normalize() {
                                        Type::Param(..) => true,
                                        _ => false,
                                    }) {
                                        prev.types.remove(index);
                                    }

                                    prev.types.push(ty);
                                    prev.fix();
                                }
                                _ => {
                                    unreachable!()
                                }
                            }

                            prev.make_cheap();
                        }
                    }
                    Entry::Vacant(mut e) => {
                        e.insert(ty);
                    }
                }

                return;
            }
            _ => {}
        }
        match self.types.entry(name.clone()) {
            Entry::Occupied(mut e) => {
                let prev = e.get_mut();
                slog::debug!(
                    self.logger,
                    "Scope.register_type({}): override = {:?}; prev = {:?}; new_ty = {:?}",
                    name,
                    should_override,
                    prev,
                    ty,
                );
                if should_override {
                    *prev = ty;
                    return;
                };

                if prev.normalize().is_intersection_type() {
                    match prev.normalize_mut() {
                        Type::Intersection(prev) => {
                            prev.types.push(ty);
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                    prev.make_cheap();
                } else {
                    let prev_ty = replace(prev, Type::any(DUMMY_SP));
                    *prev = Type::Intersection(Intersection {
                        span: DUMMY_SP,
                        types: vec![prev_ty, ty],
                    })
                    .cheap();
                }
            }
            Entry::Vacant(e) => {
                slog::debug!(self.logger, "Scope.register_type({}): {:?}", name, should_override);
                e.insert(ty);
            }
        }
    }

    pub fn this(&self) -> Option<Cow<Type>> {
        if let Some(ref this) = self.this {
            return Some(Cow::Borrowed(this));
        }

        self.parent?.this()
    }

    pub fn get_var(&self, sym: &Id) -> Option<&VarInfo> {
        if let Some(ref v) = self.vars.get(sym) {
            return Some(v);
        }

        self.search_parent(sym)
    }

    pub fn search_parent(&self, sym: &Id) -> Option<&VarInfo> {
        let mut parent = self.parent;

        while let Some(p) = parent {
            if let Some(var_info) = p.vars.get(sym) {
                return Some(var_info);
            }

            parent = p.parent;
        }

        None
    }
}

impl Analyzer<'_, '_> {
    /// Overrides a variable. Used for updating types.
    pub(super) fn override_var(&mut self, kind: VarKind, name: Id, ty: Type) -> ValidationResult<()> {
        self.declare_var(ty.span(), kind, name, Some(ty), None, true, true, true)?;

        Ok(())
    }

    /// Expands
    ///
    ///   - Type alias
    pub(super) fn expand(&mut self, span: Span, ty: Type) -> ValidationResult {
        if !self.is_builtin {
            debug_assert_ne!(
                span, DUMMY_SP,
                "expand: {:#?} cannot be expanded because it has empty span",
                ty
            );
        }

        ty.assert_valid();

        let _ctx = context(format!("expand: {}", dump_type_as_string(&self.cm, &ty)));
        let orig = dump_type_as_string(&self.cm, &ty);

        let mut v = Expander {
            logger: self.logger.clone(),
            span,
            analyzer: self,
            dejavu: Default::default(),
            full: false,
            expand_union: false,
            expand_top_level: true,
        };

        let ty = ty.foldable().fold_with(&mut v).fixed();
        ty.assert_valid();

        let new = dump_type_as_string(&self.cm, &ty);
        slog::debug!(self.logger, "[expander] expand_fully: {} => {}", orig, new);

        Ok(ty)
    }

    /// Expands
    ///
    /// // TODO: Add an option to expand union (this is required to assign)
    ///
    ///
    ///  - `expand_union` should be true if you are going to use it in
    ///    assignment, and false if you are going to use it in user-visible
    ///    stuffs (e.g. type annotation for .d.ts file)
    pub(super) fn expand_fully(&mut self, span: Span, ty: Type, expand_union: bool) -> ValidationResult {
        ty.assert_valid();
        if !self.is_builtin {
            debug_assert_ne!(
                span, DUMMY_SP,
                "expand: {:#?} cannot be expanded because it has dummy span",
                ty
            );
        }

        ty.assert_valid();

        let _ctx = context(format!("expand_fully: {}", dump_type_as_string(&self.cm, &ty)));
        let orig = dump_type_as_string(&self.cm, &ty);

        let mut v = Expander {
            logger: self.logger.clone(),
            span,
            analyzer: self,
            dejavu: Default::default(),
            full: true,
            expand_union,
            expand_top_level: true,
        };

        let ty = ty.foldable().fold_with(&mut v).fixed();
        ty.assert_valid();

        let new = dump_type_as_string(&self.cm, &ty);
        slog::debug!(self.logger, "[expander] expand_fully: {} => {}", orig, new);

        Ok(ty)
    }

    pub(super) fn expand_type_params_using_scope(&mut self, ty: Type) -> ValidationResult {
        let type_params = take(&mut self.scope.type_params);
        let res = self.expand_type_params(&type_params, ty);
        self.scope.type_params = type_params;

        res
    }

    pub(crate) fn expand_top_ref<'a>(&mut self, span: Span, ty: Cow<'a, Type>) -> ValidationResult<Cow<'a, Type>> {
        ty.assert_valid();

        if !ty.normalize().is_ref_type() {
            return Ok(ty);
        }

        let ctx = Ctx {
            preserve_ref: false,
            ignore_expand_prevention_for_top: true,
            ignore_expand_prevention_for_all: false,
            preserve_params: true,
            preserve_ret_ty: true,
            ..self.ctx
        };
        self.with_ctx(ctx)
            .expand_fully(span, ty.into_owned(), true)
            .map(Cow::Owned)
    }

    /// This should be called after calling `register_type`.

    pub(crate) fn store_unmergedable_type_span(&mut self, id: Id, span: Span) {
        if self.is_builtin {
            return;
        }

        let v = self.data.unmergable_type_decls.entry(id.clone()).or_default();
        v.push(span);

        if v.len() >= 2 {
            for span in v.iter().copied() {
                self.storage.report(Error::DuplicateName { span, name: id.clone() })
            }
        }
    }

    pub(super) fn register_type(&mut self, name: Id, ty: Type) -> Type {
        slog::debug!(self.logger, "[({})/types] Registering: {:?}", self.scope.depth(), name);

        let should_check_for_mixed = match ty.normalize() {
            Type::Param(..) => false,
            _ => true,
        };
        if should_check_for_mixed {
            // Report an error for
            //
            // export type A = {}
            // type A = {}

            if self.ctx.in_export_decl {
                self.data
                    .exported_type_decls
                    .entry(name.clone())
                    .or_default()
                    .push(ty.span());

                if let Some(spans) = self.data.local_type_decls.get(&name) {
                    self.storage.report(Error::ExportMixedWithLocal { span: ty.span() });
                    for (i, span) in spans.iter().copied().enumerate() {
                        self.storage.report(Error::ExportMixedWithLocal { span });
                        if i == 0 {
                            self.data.unmergable_type_decls.remove(&name);
                        }
                    }
                }
            } else {
                self.data
                    .local_type_decls
                    .entry(name.clone())
                    .or_default()
                    .push(ty.span());

                if let Some(spans) = self.data.exported_type_decls.get(&name) {
                    self.storage.report(Error::ExportMixedWithLocal { span: ty.span() });

                    for (i, span) in spans.iter().copied().enumerate() {
                        self.storage.report(Error::ExportMixedWithLocal { span });

                        if i == 0 {
                            self.data.unmergable_type_decls.remove(&name);
                        }
                    }
                }
            }
        }

        if self.ctx.in_global {
            if !ty.normalize().is_type_param() {
                self.env.declare_global_type(name.sym().clone(), ty.clone());
            }
        }

        if self.is_builtin {
            let ty = ty.cheap();

            self.storage
                .store_private_type(ModuleId::builtin(), name.clone(), ty.clone(), false);
            self.scope.register_type(name, ty.clone(), false);

            ty
        } else {
            let ty = ty.cheap();
            let (ty, should_override) = self
                .merge_decl_with_name(name.clone(), ty.clone())
                .map(|(ty, should_override)| (ty.cheap(), should_override))
                .unwrap_or_else(|err| {
                    self.storage.report(err);
                    (ty, false)
                });

            // Override class definitions.
            if should_override {
                if let Some(kind) = self.scope.get_var(&name).map(|v| v.kind) {
                    self.override_var(kind, name.clone(), ty.clone())
                        .report(&mut self.storage);
                }
            }

            if (self.scope.is_root() || self.scope.is_module()) && !ty.normalize().is_type_param() {
                self.storage
                    .store_private_type(self.ctx.module_id, name.clone(), ty.clone(), should_override);

                match *name.sym() {
                    js_word!("Object")
                    | js_word!("Function")
                    | js_word!("Array")
                    | js_word!("Number")
                    | js_word!("Boolean")
                    | js_word!("String") => {
                        self.env.declare_global_type(name.sym().clone(), ty.clone());
                    }
                    _ => match &**name.sym() {
                        "SymbolConstructor" => {
                            self.env.declare_global_type(name.sym().clone(), ty.clone());
                        }
                        _ => {}
                    },
                }
            }

            self.scope.register_type(name, ty.clone(), should_override);

            ty
        }
    }

    pub fn declare_vars(&mut self, kind: VarKind, pat: &RPat) -> ValidationResult<()> {
        self.declare_vars_inner_with_ty(kind, pat, None, None, None)
    }

    pub fn declare_vars_with_ty(
        &mut self,
        kind: VarKind,
        pat: &RPat,
        ty: Option<Type>,
        actual_ty: Option<Type>,
        default_ty: Option<Type>,
    ) -> ValidationResult<()> {
        self.declare_vars_inner_with_ty(kind, pat, ty, actual_ty, default_ty)
    }

    pub(super) fn declare_vars_inner(&mut self, kind: VarKind, pat: &RPat) -> ValidationResult<()> {
        self.declare_vars_inner_with_ty(kind, pat, None, None, None)
    }

    pub(super) fn resolve_typeof(&mut self, span: Span, name: &RTsEntityName) -> ValidationResult {
        if !self.is_builtin {
            debug_assert!(!span.is_dummy(), "Cannot resolve `typeof` with a dummy span");
        }

        let mut ty = match name {
            RTsEntityName::Ident(i) => {
                if i.sym == js_word!("undefined") {
                    return Ok(Type::any(span));
                }
                let mut i = i.clone();
                if i.span.is_dummy() {
                    i.span = span;
                }
                self.type_of_var(&i, TypeOfMode::RValue, None)?
            }
            RTsEntityName::TsQualifiedName(n) => {
                let ctx = Ctx {
                    allow_module_var: true,
                    ..self.ctx
                };
                let obj = self
                    .with_ctx(ctx)
                    .resolve_typeof(span, &n.left)
                    .context("tried to resolve lhs of typeof")?;
                let i = &n.right;

                self.access_property(
                    span,
                    &obj,
                    &Key::Normal {
                        span: i.span,
                        sym: i.sym.clone(),
                    },
                    TypeOfMode::RValue,
                    IdCtx::Var,
                )?
            }
        };
        ty.reposition(span);
        Ok(ty)
    }

    #[inline(never)]
    pub(super) fn find_var(&self, name: &Id) -> Option<&VarInfo> {
        static ANY_VAR: Lazy<VarInfo> = Lazy::new(|| VarInfo {
            ty: Some(Type::any(DUMMY_SP)),
            actual_ty: Some(Type::any(DUMMY_SP)),
            kind: VarKind::Error,
            initialized: true,
            copied: false,
            is_actual_type_modified_in_loop: false,
        });

        let mut scope = Some(&self.scope);

        while let Some(s) = scope {
            if let Some(var) = s.vars.get(name) {
                return Some(var);
            }
            if let Some(ref cls) = s.this_class_name {
                if *cls == *name {
                    return Some(&ANY_VAR);
                }
            }

            scope = s.parent;
        }

        None
    }

    pub(super) fn find_var_type(&self, name: &Id, mode: TypeOfMode) -> Option<Cow<Type>> {
        if let Some(v) = self.cur_facts.true_facts.vars.get(&Name::from(name)) {
            v.assert_valid();
            return Some(Cow::Borrowed(v));
        }

        // println!("({}) find_var_type({})", self.scope.depth(), name);
        let mut scope = Some(&self.scope);
        while let Some(s) = scope {
            if let Some(ref v) = s.facts.vars.get(&Name::from(name)) {
                v.assert_valid();

                slog::debug!(self.logger, "Scope.find_var_type({}): Handled from facts", name);
                return Some(Cow::Borrowed(v));
            }

            scope = s.parent;
        }

        {
            // Improted variables
            if let Some(info) = self.imports_by_id.get(name) {
                if let Some(var_ty) = info.data.vars.get(name.sym()) {
                    var_ty.assert_valid();

                    slog::debug!(self.logger, "Scope.find_var_type({}): Handled with imports", name);
                    return Some(Cow::Borrowed(var_ty));
                }
            }
        }

        if let Some(var) = self.find_var(name) {
            slog::debug!(
                self.logger,
                "({}) find_var_type({}): Handled from scope.find_var",
                self.scope.depth(),
                name
            );

            let name = Name::from(name);

            let mut ty = match mode {
                TypeOfMode::LValue => match &var.ty {
                    Some(ty) => ty.clone(),
                    _ => return None,
                },
                TypeOfMode::RValue => match &var.actual_ty {
                    Some(ty) => ty.clone(),
                    _ => return None,
                },
            };
            ty.assert_valid();

            if let Some(ref excludes) = self.scope.facts.excludes.get(&name) {
                match ty.normalize_mut() {
                    Type::Union(ty::Union { ref mut types, .. }) => {
                        for ty in types {
                            let span = (*ty).span();
                            for excluded_ty in excludes.iter() {
                                if ty.type_eq(excluded_ty) {
                                    *ty = Type::never(span)
                                }
                            }
                        }
                    }
                    _ => {}
                }

                ty.fix();
            }

            return Some(Cow::Owned(ty));
        }

        {
            if let Some(ty) = self.storage.get_local_var(self.ctx.module_id, name.clone()) {
                ty.assert_valid();

                slog::debug!(self.logger, "Scope.find_var_type({}): Handled with storage", name);
                return Some(Cow::Owned(ty));
            }
        }

        None
    }

    pub fn find_type(&self, target: ModuleId, name: &Id) -> ValidationResult<Option<ItemRef<Type>>> {
        if target == self.ctx.module_id || target.is_builtin() {
            if let Some(v) = self.find_local_type(name) {
                return Ok(Some(v));
            }
        }

        if let Some(ModuleInfo { data, .. }) = self.imports_by_id.get(name) {
            if let Some(types) = data.types.get(name.sym()) {
                let types = types.clone();
                return Ok(Some(ItemRef::Owned(types.into_iter())));
            }
        }

        if let Ok(ty) = self.env.get_global_type(DUMMY_SP, &name.sym()) {
            return Ok(Some(ItemRef::Owned(vec![ty].into_iter())));
        }

        if let Some(data) = self.imports.get(&(self.ctx.module_id, target)) {
            if let Some(types) = data.types.get(name.sym()) {
                let types = types.clone();
                return Ok(Some(ItemRef::Owned(types.into_iter())));
            }
            if let Some(types) = data.private_types.get(name) {
                let types = types.clone();
                return Ok(Some(ItemRef::Owned(types.into_iter())));
            }
        }

        Ok(None)
    }

    fn find_local_type(&self, name: &Id) -> Option<ItemRef<Type>> {
        #[allow(dead_code)]
        static ANY: Type = Type::Keyword(RTsKeywordType {
            span: DUMMY_SP,
            kind: TsKeywordTypeKind::TsAnyKeyword,
        });
        #[allow(dead_code)]
        static STATIC_THIS: Type = Type::StaticThis(StaticThis { span: DUMMY_SP });

        if let Some(class) = &self.scope.get_this_class_name() {
            if *class == *name {
                // TODO: Maybe change this to special variant.
                return Some(ItemRef::Single(iter::once(&STATIC_THIS)));
            }
        }

        slog::debug!(self.logger, "({}) Analyzer.find_type(`{}`)", self.scope.depth(), name);

        let mut src = vec![];
        if !self.is_builtin {
            if let Ok(ty) = self.env.get_global_type(DUMMY_SP, name.sym()) {
                debug_assert!(ty.is_clone_cheap(), "{:?}", ty);

                slog::debug!(
                    self.logger,
                    "Using builtin / global type: {}",
                    dump_type_as_string(&self.cm, &ty)
                );
                src.push(ty.clone());
            }
        }

        if let Some(ty) = self.scope.find_type(name) {
            slog::debug!(self.logger, "Using type from scope: {:?}", ty);
            src.extend(ty.into_iter().map(Cow::into_owned));
            return Some(ItemRef::Owned(
                vec![Type::intersection(DUMMY_SP, src).cheap()].into_iter(),
            ));
        }

        if let Some(ty) = self.storage.get_local_type(self.ctx.module_id, name.clone()) {
            return Some(ItemRef::Owned(vec![ty].into_iter()));
        }

        if !self.is_builtin {
            slog::debug!(self.logger, "Scope.find_type: failed to find type '{}'", name);
        }

        None
    }

    /// TODO: Restore this(?)
    pub(super) fn mark_var_as_truthy(&mut self, name: Id) -> ValidationResult<()> {
        self.modify_var(name, |var| {
            // var.ty = var.ty.take().map(|ty| ty.remove_falsy());
            Ok(())
        })
    }

    fn modify_var<F, Ret>(&mut self, name: Id, op: F) -> ValidationResult<Ret>
    where
        F: FnOnce(&mut VarInfo) -> ValidationResult<Ret>,
    {
        let var = self.find_var(&name);
        let ty = var.and_then(|var| var.ty.clone());

        if let Some(ty) = &ty {
            ty.assert_valid();
        }

        op(self.scope.vars.entry(name).or_insert_with(|| VarInfo {
            kind: VarKind::Error,
            initialized: true,
            ty: ty.clone(),
            actual_ty: ty,
            copied: true,
            is_actual_type_modified_in_loop: false,
        }))
    }

    /// If `allow_multiple` is true and `is_override` is false, the value type
    /// is updated only if it's temporary type (like `typeof foo` while
    /// validating `foo`).
    pub fn declare_var(
        &mut self,
        span: Span,
        kind: VarKind,
        name: Id,
        ty: Option<Type>,
        actual_ty: Option<Type>,
        initialized: bool,
        allow_multiple: bool,
        is_override: bool,
    ) -> ValidationResult<()> {
        let marks = self.marks();

        if let Some(ty) = &ty {
            ty.assert_valid();
            slog::debug!(
                self.logger,
                "[({})/vars]: Declaring {} as {}",
                self.scope.depth(),
                name,
                dump_type_as_string(&self.cm, ty)
            );
        } else {
            slog::debug!(
                self.logger,
                "[({})/vars]: Declaring {} without type",
                self.scope.depth(),
                name,
            );
        }

        if let Some(ty) = &actual_ty {
            ty.assert_valid();
        }

        if !self.is_builtin
            && !is_override
            && !allow_multiple
            && !self.ctx.ignore_errors
            && !self.ctx.reevaluating()
            && !self.ctx.in_ts_fn_type
        {
            let spans = self.data.var_spans.entry(name.clone()).or_default();
            let err = !spans.is_empty();

            spans.push(span);

            if err {
                for &span in &**spans {
                    self.storage.report(Error::DuplicateVar {
                        name: name.clone(),
                        span,
                    });
                }
            }
        }

        match kind {
            VarKind::Decl(VarDeclKind::Let | VarDeclKind::Const) => {
                if *name.sym() == js_word!("let") || *name.sym() == js_word!("const") {
                    self.storage
                        .report(Error::LetOrConstIsNotValidIdInLetOrConstVarDecls { span });
                }
            }
            _ => {}
        }

        let ty = match &ty {
            Some(..) if self.is_builtin => ty,
            Some(t) => {
                // If type is not found, we use `any`.
                match self.expand_top_ref(ty.span(), Cow::Borrowed(t)) {
                    Ok(new_ty) => {
                        if new_ty.is_any() {
                            Some(new_ty.into_owned())
                        } else {
                            ty
                        }
                    }
                    Err(..) => Some(Type::any(ty.span())),
                }
            }
            None => None,
        };

        if let Some(ty) = &ty {
            slog::debug!(
                self.logger,
                "[vars]: Expanded {} as {}",
                name,
                dump_type_as_string(&self.cm, ty)
            );
        }

        let ty = ty.map(|ty| ty.cheap());

        if let Some(actual_ty) = &actual_ty {
            if actual_ty.is_never() {
                print_backtrace();
            }
        }
        let actual_ty = actual_ty
            .and_then(|ty| {
                if ty.is_any()
                    || ty.is_kwd(TsKeywordTypeKind::TsUndefinedKeyword)
                    || ty.is_kwd(TsKeywordTypeKind::TsNullKeyword)
                    || ty.is_unknown()
                {
                    None
                } else {
                    Some(ty)
                }
            })
            .map(|ty| ty.cheap());

        if let Some(ty) = &actual_ty {
            ty.assert_valid();
        }

        if self.ctx.in_global {
            if let Some(ty) = ty.clone() {
                self.env.declare_global_var(name.sym().clone(), ty.clone());
            }
        }

        if self.scope.is_root() || self.scope.is_module() {
            self.storage.store_private_var(
                self.ctx.module_id,
                name.clone(),
                ty.clone().unwrap_or_else(|| Type::any(span)),
            )
        }

        match self.scope.vars.entry(name.clone()) {
            Entry::Occupied(e) => {
                //println!("\tdeclare_var: found entry");
                let (k, mut v) = e.remove_entry();

                macro_rules! restore {
                    () => {{
                        self.scope.vars.insert(k, v);
                    }};
                }

                if let Some(ty) = &v.ty {
                    ty.assert_valid();
                }
                if let Some(ty) = &v.actual_ty {
                    ty.assert_valid();
                }

                if !self.is_builtin && is_override {
                    v.ty = ty;
                    return Ok(());
                }

                if !self.data.known_wrong_overloads.contains(&name) {
                    if let Some(orig) = &v.ty {
                        if let Some(ty) = &ty {
                            self.validate_with(|a| {
                                let res = a.validate_fn_overloads(span, orig, ty);

                                if res.is_err() {
                                    a.data.known_wrong_overloads.insert(name.clone());
                                }

                                res
                            });
                        }
                    }
                }

                v.ty = if let Some(ty) = ty {
                    Some(if let Some(var_ty) = v.ty {
                        match ty.normalize() {
                            Type::Union(..) => {
                                // TODO: Check if all types are query or
                                // function
                            }
                            Type::Query(..) | Type::Function(..) => {}
                            Type::Module(..) => {
                                unreachable!("module is not a variable")
                            }
                            _ => {
                                let generalized_var_ty = var_ty.clone().generalize_lit(marks);

                                match var_ty.normalize() {
                                    // Allow overriding query type.
                                    Type::Query(..) => {}
                                    // Allow overloading query type.
                                    Type::Function(..) => {}
                                    Type::Union(..) => {
                                        // TODO: Check if all types are query or
                                        // function
                                    }

                                    _ => {
                                        let ty = self.expand_fully(span, ty.clone(), true)?;
                                        let var_ty = self.expand_fully(span, generalized_var_ty, true)?;

                                        let res = self.assign(&mut Default::default(), &ty, &var_ty, span);

                                        if let Err(err) = res {
                                            self.storage.report(err);
                                            v.ty = Some(var_ty);
                                            restore!();
                                            return Ok(());

                                            // TODO:
                                            //  return Err(Error::
                                            //      RedeclaredVarWithDifferentType {
                                            //          span,
                                            //      }
                                            //  );
                                        }
                                    }
                                }
                            }
                        }
                        if var_ty.type_eq(&ty) {
                            var_ty
                        } else {
                            Type::union(vec![var_ty, ty])
                        }
                    } else {
                        ty
                    })
                } else {
                    if let Some(var_ty) = v.ty {
                        Some(var_ty)
                    } else {
                        None
                    }
                };
                if let Some(ty) = &actual_ty {
                    ty.assert_valid();
                }
                if let Some(ty) = &v.ty {
                    ty.assert_valid();
                }
                // TODO: Use better logic
                v.actual_ty = actual_ty.or_else(|| v.ty.clone());

                self.scope.vars.insert(k, v);
            }
            Entry::Vacant(e) => {
                //println!("\tdeclare_var: no entry");

                let info = VarInfo {
                    kind,
                    ty: ty.clone(),
                    actual_ty: actual_ty.or_else(|| ty.clone()),
                    initialized,
                    copied: false,
                    is_actual_type_modified_in_loop: false,
                };
                e.insert(info);
            }
        }

        Ok(())
    }

    /// Returns [Err] if overload is wrong.
    fn validate_fn_overloads(&mut self, span: Span, orig: &Type, new: &Type) -> ValidationResult<()> {
        // We validates using the signature of implementing function.
        // TODO: Validate using last element, when there's a no function decl with body.
        if self.is_builtin || self.ctx.in_declare {
            return Ok(());
        }

        for orig in orig.iter_union() {
            match orig.normalize() {
                Type::Function(..) => {
                    self.assign_with_opts(
                        &mut Default::default(),
                        AssignOpts {
                            span,
                            for_overload: true,
                            ..Default::default()
                        },
                        &new,
                        &orig,
                    )
                    .convert_err(|err| Error::ImcompatibleFnOverload {
                        span: orig.span(),
                        cause: box err,
                    })
                    .context("tried to validate signatures of overloaded functions")?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// TODO: Merge with declare_vars_*
    pub fn declare_complex_vars(
        &mut self,
        kind: VarKind,
        pat: &RPat,
        ty: Type,
        actual_ty: Option<Type>,
        default_ty: Option<Type>,
    ) -> ValidationResult<()> {
        match pat {
            RPat::Assign(..) | RPat::Ident(..) | RPat::Array(..) | RPat::Object(..) | RPat::Rest(..) => self.add_vars(
                pat,
                Some(ty),
                actual_ty,
                default_ty,
                DeclareVarsOpts {
                    kind,
                    use_iterator_for_array: true,
                },
            ),

            _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
        }
    }

    pub(crate) fn contains_infer_type<T>(&self, ty: &T) -> bool
    where
        T: VisitWith<MarkFinder>,
    {
        swc_common::GLOBALS.set(self.env.shared().swc_globals(), || {
            contains_mark(ty, self.marks().contains_infer_type_mark)
        })
    }

    pub(crate) fn mark_as_infer_type_container(&self, span: Span) -> Span {
        span.apply_mark(self.marks().contains_infer_type_mark)
    }

    fn is_infer_type_container(&self, ty: &Type) -> bool {
        let mut ctxt: SyntaxContext = ty.span().ctxt();
        loop {
            let mark = ctxt.remove_mark();

            if mark == Mark::root() {
                break;
            }

            if mark == self.marks().contains_infer_type_mark {
                return true;
            }
        }

        false
    }

    pub(crate) fn mark_type_as_infer_type_container(&self, ty: &mut Type) {
        let span = ty.span();
        let span = self.mark_as_infer_type_container(span);
        ty.respan(span);
    }

    pub(crate) fn prevent_expansion<T>(&self, ty: &mut T)
    where
        T: VisitMutWith<ExpansionPreventer>,
    {
        if self.is_builtin {
            return;
        }

        ty.visit_mut_with(&mut ExpansionPreventer {
            mark: self.marks().no_expand_mark,
        });
    }

    pub(crate) fn allow_expansion<T>(&self, ty: &mut T)
    where
        T: VisitMutWith<ExpansionPreventer>,
    {
        if self.is_builtin {
            return;
        }

        ty.visit_mut_with(&mut ExpansionPreventer {
            mark: self.marks().ignore_no_expand_mark,
        });
    }

    pub(super) fn is_expansion_prevented(&self, ty: &Type) -> bool {
        let mut found_no_expand = false;
        let mut ctxt: SyntaxContext = ty.span().ctxt();
        loop {
            let mark = ctxt.remove_mark();

            if mark == Mark::root() {
                break;
            }

            if mark == self.marks().no_expand_mark {
                found_no_expand = true;
                continue;
            }

            if mark == self.marks().ignore_no_expand_mark {
                return false;
            }
        }

        found_no_expand
    }
}

#[derive(Debug, Clone)]
pub(crate) struct VarInfo {
    pub kind: VarKind,
    pub initialized: bool,

    /// Declared type.
    pub ty: Option<Type>,

    /// Stored type.
    pub actual_ty: Option<Type>,

    /// Copied from parent scope. If this is true, it's not a variable
    /// declaration.
    pub copied: bool,

    /// If this is true, types will become union while moving variables to
    /// parent scope.
    pub is_actual_type_modified_in_loop: bool,
}

impl<'a> Scope<'a> {
    pub const fn kind(&self) -> ScopeKind {
        self.kind
    }

    /// Returns true if `this` (from javascript) is a reference to an object
    /// literal.
    pub fn is_this_ref_to_object_lit(&self) -> bool {
        match self.kind {
            ScopeKind::Fn => {}
            // An arrow function does not modified `this.`
            ScopeKind::ArrowFn => {}

            // `this` in object literal resolves to the object literal itself.
            ScopeKind::ObjectLit => return true,

            ScopeKind::Class => return false,
            ScopeKind::TypeParams
            | ScopeKind::Call
            | ScopeKind::Method { .. }
            | ScopeKind::Constructor
            | ScopeKind::Flow
            | ScopeKind::Block
            | ScopeKind::Module
            | ScopeKind::LoopBody { .. } => {}
        }

        match self.parent {
            Some(parent) => parent.is_this_ref_to_object_lit(),
            None => false,
        }
    }

    /// Returns true if `this` (from javascript) is a reference to a class.
    pub fn is_this_ref_to_class(&self) -> bool {
        match self.kind {
            ScopeKind::Module => return false,

            ScopeKind::Fn => return false,
            // An arrow function does not modified `this.`
            ScopeKind::ArrowFn => {}

            // `this` in object literal resolves to the object literal itself.
            ScopeKind::ObjectLit => return false,

            ScopeKind::Class => return true,
            ScopeKind::TypeParams
            | ScopeKind::Call
            | ScopeKind::Method { .. }
            | ScopeKind::Constructor
            | ScopeKind::Flow
            | ScopeKind::Block
            | ScopeKind::LoopBody { .. } => {}
        }

        match self.parent {
            Some(parent) => parent.is_this_ref_to_class(),
            None => false,
        }
    }

    pub fn new(parent: &'a Scope<'a>, kind: ScopeKind, facts: CondFacts) -> Self {
        Self::new_inner(parent.logger.clone(), Some(parent), kind, facts)
    }

    pub fn root(logger: Logger) -> Self {
        Self::new_inner(logger, None, ScopeKind::Fn, Default::default())
    }

    fn new_inner(logger: Logger, parent: Option<&'a Scope<'a>>, kind: ScopeKind, facts: CondFacts) -> Self {
        Scope {
            logger,
            parent,
            kind,
            declaring: Default::default(),
            declared_return_type: None,
            declaring_type_params: Default::default(),
            vars: Default::default(),
            types: Default::default(),
            facts,
            declaring_fn: None,
            declaring_prop: None,
            this: None,
            this_class_name: Default::default(),
            this_class_members: Default::default(),
            this_object_members: Default::default(),
            super_class: None,
            return_values: Default::default(),
            expand_triage_depth: 0,
            is_call_arg_count_unknown: false,
            type_params: Default::default(),
            cur_module_name: None,
            class: Default::default(),
        }
    }

    pub(super) fn depth(&self) -> usize {
        match self.parent {
            Some(ref p) => p.depth() + 1,
            None => 0,
        }
    }

    /// This method does **not** handle imported types.
    fn find_type(&self, name: &Id) -> Option<ItemRef<Type>> {
        slog::debug!(self.logger, "Analyzer.find_type('{}')", name);

        if let Some(ty) = self.facts.types.get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", ty);
            // println!("({}) find_type({}): Found (cond facts)", self.depth(), name);
            return Some(ItemRef::Single(iter::once(&ty)));
        }

        if let Some(ty) = self.types.get(name) {
            debug_assert!(ty.is_clone_cheap(), "{:?}", ty);

            // println!("({}) find_type({}): Found", self.depth(), name);

            return Some(ItemRef::Single(once(&*ty)));
        }

        match self.parent {
            Some(ref parent) => parent.find_type(name),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemRef<'a, T: Clone> {
    Single(iter::Once<&'a T>),
    Multi(slice::Iter<'a, T>),
    Owned(std::vec::IntoIter<T>),
}

impl<'a, T> Iterator for ItemRef<'a, T>
where
    T: Clone,
{
    type Item = Cow<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self {
            ItemRef::Single(v) => Cow::Borrowed(v.next()?),
            ItemRef::Multi(v) => Cow::Borrowed(v.next()?),
            ItemRef::Owned(v) => Cow::Owned(v.next()?),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeKind {
    Block,
    Fn,
    /// This does not affect `this`.
    Method {
        is_static: bool,
    },
    /// This is different from method because of `super`.
    ///
    /// See: computedPropertyNames30_ES5.ts
    Constructor,
    ArrowFn,
    /// This variant is related to handling of `this.foo()` in class methods.
    Class,
    /// This variant is related to handling of `this.foo()` in method
    /// properties.
    ObjectLit,
    /// If statement, conditional expression, switch case
    Flow,
    /// Scope to store type parameters.
    TypeParams,
    /// Type parameters are stored in this scope.
    Call,
    Module,
    /// Used to capture type facts created by loop bodies.
    LoopBody {
        last: bool,
    },
}

impl ScopeKind {
    /// TODO: Change
    pub fn allows_respanning(self) -> bool {
        match self {
            ScopeKind::Flow | ScopeKind::Class | ScopeKind::ObjectLit => false,
            _ => true,
        }
    }
}

struct Expander<'a, 'b, 'c> {
    logger: Logger,
    span: Span,
    analyzer: &'a mut Analyzer<'b, 'c>,
    dejavu: FxHashSet<Id>,
    full: bool,
    expand_union: bool,
    /// Should we expand top level references?
    expand_top_level: bool,
}

impl Expander<'_, '_, '_> {
    fn expand_ts_entity_name(
        &mut self,
        span: Span,
        ctxt: ModuleId,
        type_name: &RTsEntityName,
        type_args: Option<&TypeParamInstantiation>,
        was_top_level: bool,
        trying_primitive_expansion: bool,
    ) -> ValidationResult<Option<Type>> {
        macro_rules! verify {
            ($ty:expr) => {{
                if cfg!(debug_assertions) {
                    match $ty.normalize() {
                        Type::Ref(ref s) => unreachable!("ref: {:?}", s),
                        _ => {}
                    }
                }
            }};
        }

        match type_name {
            RTsEntityName::Ident(ref i) => {
                if let Some(class) = &self.analyzer.scope.get_this_class_name() {
                    if *class == *i {
                        return Ok(None);
                    }
                }
                if i.sym == js_word!("void") {
                    return Ok(Some(Type::any(span)));
                }

                slog::info!(self.logger, "Info: {}{:?}", i.sym, i.span.ctxt);
                if !trying_primitive_expansion && self.dejavu.contains(&i.into()) {
                    slog::error!(self.logger, "Dejavu: {}{:?}", &i.sym, i.span.ctxt);
                    return Ok(None);
                }
                if let Some(types) = self.analyzer.find_type(ctxt, &i.into())? {
                    slog::info!(
                        self.logger,
                        "expand: expanding `{}` using analyzer: {}",
                        Id::from(i),
                        types.clone().into_iter().count()
                    );

                    let mut stored_ref = None;

                    for t in types {
                        if !self.expand_union {
                            let mut finder = UnionFinder { found: false };
                            t.visit_with(&mut finder);
                            if finder.found {
                                return Ok(None);
                            }
                        }
                        // We should expand alias again.
                        let is_alias = match t.normalize() {
                            Type::Alias(..) => true,
                            _ => false,
                        };

                        match t.normalize() {
                            Type::Intersection(..) => return Ok(Some(t.into_owned().clone())),

                            // Result of type expansion should not be Ref unless really required.
                            Type::Ref(r) => {
                                let r = r.clone();
                                // TODO: Handle type args

                                return self.expand_ref(r, was_top_level);
                            }

                            ty @ Type::Enum(..) => {
                                if let Some(..) = type_args {
                                    Err(Error::NotGeneric { span })?;
                                }
                                verify!(ty);
                                return Ok(Some(ty.clone()));
                            }

                            ty @ Type::Param(..) => {
                                if let Some(..) = type_args {
                                    Err(Error::NotGeneric { span })?;
                                }

                                verify!(ty);
                                return Ok(Some(ty.clone()));
                            }

                            Type::Interface(Interface { type_params, .. })
                            | Type::Alias(Alias { type_params, .. })
                            | Type::Class(Class {
                                def: box ClassDef { type_params, .. },
                                ..
                            })
                            | Type::ClassDef(ClassDef { type_params, .. }) => {
                                let ty = t.clone().into_owned();
                                let type_params = type_params.clone();

                                if let Some(type_params) = type_params {
                                    let type_args: Option<_> = type_args.cloned().fold_with(self);

                                    slog::info!(self.logger, "expand: expanding type parameters");
                                    let mut inferred = self.analyzer.infer_arg_types(
                                        self.span,
                                        type_args.as_ref(),
                                        &type_params.params,
                                        &[],
                                        &[],
                                        Some(&Type::TypeLit(TypeLit {
                                            span,
                                            members: vec![],
                                            metadata: Default::default(),
                                        })),
                                    )?;
                                    inferred.iter_mut().for_each(|(_, ty)| {
                                        self.analyzer.allow_expansion(ty);
                                    });

                                    let before = dump_type_as_string(&self.analyzer.cm, &ty);
                                    let mut ty = self.analyzer.expand_type_params(&inferred, ty.foldable())?;
                                    let after = dump_type_as_string(&self.analyzer.cm, &ty);
                                    slog::debug!(
                                        &self.analyzer.logger,
                                        "[expand] Expanded generics: {} => {}",
                                        before,
                                        after
                                    );

                                    match ty {
                                        Type::ClassDef(def) => {
                                            ty = Type::Class(Class {
                                                span: self.span,
                                                def: box def,
                                            });
                                        }
                                        _ => {}
                                    };

                                    if is_alias {
                                        self.dejavu.insert(i.into());
                                        ty = ty.fold_with(self);
                                        self.dejavu.remove(&i.into());
                                    }

                                    return Ok(Some(ty));
                                }

                                match ty.normalize() {
                                    Type::Interface(..) if !trying_primitive_expansion && was_top_level => {
                                        return Ok(Some(ty.clone()));
                                    }
                                    _ => {}
                                }

                                match ty.normalize() {
                                    Type::Alias(..) => {
                                        self.expand_top_level = true;
                                    }
                                    _ => {}
                                }

                                let mut ty = ty.foldable();

                                if is_alias {
                                    self.dejavu.insert(i.into());
                                    ty = ty.fold_with(self);
                                    self.dejavu.remove(&i.into());
                                }

                                match ty {
                                    Type::ClassDef(def) => {
                                        ty = Type::Class(Class {
                                            span: self.span,
                                            def: box def,
                                        });
                                    }
                                    _ => {}
                                };

                                return Ok(Some(ty));
                            }

                            Type::Mapped(m) => {}

                            _ => stored_ref = Some(t),
                        }
                    }

                    if let Some(t) = stored_ref {
                        self.expand_top_level = true;
                        return Ok(Some(t.into_owned().foldable().fold_with(self).fixed()));
                    }
                }

                if i.sym == *"undefined" || i.sym == *"null" {
                    return Ok(Some(Type::any(span)));
                }

                slog::error!(
                    self.logger,
                    "({}) Failed to find type: {}{:?}",
                    self.analyzer.scope.depth(),
                    i.sym,
                    i.span.ctxt
                );
            }

            // Handle enum variant type.
            //
            //  let a: StringEnum.Foo = x;
            RTsEntityName::TsQualifiedName(box RTsQualifiedName { left, ref right, .. }) => {
                let left =
                    self.expand_ts_entity_name(span, ctxt, left, None, was_top_level, trying_primitive_expansion)?;

                if let Some(left) = &left {
                    let ty = self
                        .analyzer
                        .access_property(
                            span,
                            left,
                            &Key::Normal {
                                span,
                                sym: right.sym.clone(),
                            },
                            TypeOfMode::RValue,
                            IdCtx::Type,
                        )
                        .report(&mut self.analyzer.storage)
                        .unwrap_or_else(|| Type::any(span));
                    return Ok(Some(ty));
                }
            }
        }

        print_backtrace();

        Ok(Some(Type::any(span)))
    }
    fn expand_ref(&mut self, r: Ref, was_top_level: bool) -> ValidationResult<Option<Type>> {
        let trying_primitive_expansion = self.analyzer.scope.expand_triage_depth != 0;

        let Ref {
            span: r_span,
            ctxt,
            type_name,
            type_args,
            ..
        } = r;
        let span = self.span;

        if !trying_primitive_expansion && (!self.full || self.analyzer.ctx.preserve_ref) {
            return Ok(None);
        }

        let mut ty = self.expand_ts_entity_name(
            span,
            ctxt,
            &type_name,
            type_args.as_deref(),
            was_top_level,
            trying_primitive_expansion,
        )?;

        if let Some(ty) = &mut ty {
            ty.reposition(r_span);

            if let Type::Enum(e) = ty.normalize() {
                return Ok(Some(Type::EnumVariant(EnumVariant {
                    span,
                    ctxt,
                    enum_name: e.id.clone().into(),
                    name: None,
                })));
            }
        }

        Ok(ty)
    }

    fn expand_type(&mut self, mut ty: Type) -> Type {
        match ty {
            Type::Keyword(..) | Type::Lit(..) => return ty,
            Type::Arc(..) => {
                return ty.foldable().fold_with(self);
            }
            _ => {}
        }

        let _stack = match stack::track(self.span) {
            Ok(v) => v,
            Err(..) => {
                print_backtrace();
                slog::error!(
                    self.logger,
                    "[expander] Stack overflow: {}",
                    dump_type_as_string(&self.analyzer.cm, &ty)
                );
                return ty;
            }
        };

        self.full |= match ty {
            Type::Mapped(..) => true,
            _ => false,
        };

        let trying_primitive_expansion = self.analyzer.scope.expand_triage_depth != 0;

        // If we are trying to expand types as primitive, we ignore config
        let is_expansion_prevented = !trying_primitive_expansion && self.analyzer.is_expansion_prevented(&ty);

        if self.analyzer.scope.expand_triage_depth != 0 {
            self.analyzer.scope.expand_triage_depth += 1;
        }

        if self.analyzer.scope.expand_triage_depth > 30 {
            return ty;
        }

        // We do not expand types specified by user
        if is_expansion_prevented {
            if !self.analyzer.ctx.ignore_expand_prevention_for_all
                && !(self.expand_top_level && self.analyzer.ctx.ignore_expand_prevention_for_top)
            {
                match ty.normalize() {
                    Type::Ref(r) => {
                        // Expand type arguments if it should be expanded
                        if self.analyzer.contains_infer_type(&r.type_args) {
                            return Type::Ref(r.clone().fold_children_with(self));
                        }
                    }
                    _ => {}
                }

                if self.expand_top_level && self.analyzer.scope.expand_triage_depth == 0 {
                    let old_full = self.full;

                    self.analyzer.scope.expand_triage_depth += 1;
                    self.full = true;

                    let candidate = ty.clone().fold_with(self);

                    self.analyzer.scope.expand_triage_depth = 0;
                    self.full = old_full;

                    return candidate;
                }

                return ty;
            }
        }

        let was_top_level = self.expand_top_level;
        self.expand_top_level = false;

        // Start handling type expansion.
        let res: ValidationResult<()> = try {
            if self.analyzer.contains_infer_type(&ty) || contains_infer_type(&ty) {
                match ty.normalize_mut() {
                    Type::Conditional(cond_ty) => {
                        match cond_ty.check_type.normalize_mut() {
                            Type::Query(QueryType {
                                span,
                                expr: box QueryExpr::TsEntityName(RTsEntityName::Ident(name)),
                                ..
                            }) => {
                                let id = (&*name).into();
                                let ctxt = self.analyzer.ctx.module_id;
                                //
                                if let Some(ty) = self.analyzer.find_var_type(&id, TypeOfMode::RValue) {
                                    cond_ty.check_type = box ty.into_owned();
                                } else {
                                    slog::error!(self.analyzer.logger, "Failed to find variable named {:?}", id);
                                }
                            }

                            _ => {}
                        }
                    }

                    _ => {}
                }
            }
        };

        let ty = ty.foldable();
        match ty {
            Type::Intersection(mut i) if was_top_level => {
                i.types = i.types.move_map(|ty| {
                    self.expand_top_level = true;
                    ty.fold_with(self)
                });
                self.expand_top_level = false;
                return Type::Intersection(i);
            }
            Type::Keyword(..) => return ty,
            Type::Param(..) => return ty.fold_children_with(self),

            Type::Alias(alias) => {
                self.expand_top_level = was_top_level;
                return *alias.ty.fold_with(self);
            }

            Type::Ref(..) if !self.full => return ty.fold_children_with(self),
            Type::Interface(..) | Type::Class(..) if !self.full => return ty,
            Type::IndexedAccessType(IndexedAccessType {
                obj_type: box Type::TypeLit(lit),
                index_type:
                    box Type::Param(TypeParam {
                        constraint:
                            Some(box Type::Operator(Operator {
                                op: TsTypeOperatorOp::KeyOf,
                                ty: box Type::TypeLit(key_lits),
                                ..
                            })),
                        ..
                    }),
                readonly: false,
                ..
            }) if lit == key_lits => return Type::TypeLit(lit.fold_with(self)),

            _ => {}
        }

        if !self.expand_union {
            let mut finder = UnionFinder { found: false };
            ty.visit_with(&mut finder);
            if finder.found {
                return ty;
            }
        }

        let span = self.span;

        let mut ty = match ty {
            Type::Ref(..) => ty,
            _ => ty.fold_children_with(self),
        };

        let res: ValidationResult = try {
            match ty.normalize() {
                Type::Ref(r) => {
                    let ty = self.expand_ref(r.clone(), was_top_level)?;

                    //
                    if trying_primitive_expansion {
                        match ty {
                            Some(ty @ Type::Keyword(..)) => return ty,
                            _ => return Type::Ref(r.clone()),
                        }
                    } else {
                        match ty {
                            Some(ty) => {
                                return ty;
                            }
                            None => {}
                        }
                    }
                }

                Type::Param(..) => return ty,

                _ => {}
            }

            match ty {
                ty @ Type::TypeLit(..) => return ty,

                Type::Mapped(Mapped {
                    span,
                    type_param:
                        TypeParam {
                            span: type_param_span,
                            ref name,
                            constraint:
                                Some(box Type::Operator(Operator {
                                    op: TsTypeOperatorOp::KeyOf,
                                    ty: ref constraint,
                                    ..
                                })),
                            default: None,
                        },
                    readonly,
                    optional,
                    ty:
                        Some(box Type::Conditional(Conditional {
                            span: cond_span,
                            check_type:
                                box Type::IndexedAccessType(IndexedAccessType {
                                    span: checl_type_span,
                                    ref obj_type,
                                    index_type: box Type::Param(ref index_type),
                                    ..
                                }),
                            ref extends_type,
                            ref true_type,
                            ref false_type,
                        })),
                    ..
                }) if constraint.type_eq(&obj_type) && *name == index_type.name => {
                    let unwrap_type = |ty: &Type| match ty {
                        Type::IndexedAccessType(IndexedAccessType {
                            obj_type,
                            index_type: box Type::Param(index_type),
                            ..
                        }) if index_type.name == *name => {
                            return (true, *obj_type.clone());
                        }
                        _ => (false, ty.clone()),
                    };

                    match &**obj_type {
                        Type::Tuple(obj_type) => {
                            let elements = obj_type
                                .elems
                                .iter()
                                .cloned()
                                .enumerate()
                                .map(|(idx, mut element)| {
                                    if let Some(v) =
                                        self.analyzer
                                            .extends(span, Default::default(), &element.ty, &extends_type)
                                    {
                                        let ty = if v { true_type } else { false_type };

                                        let (unwrapped, ty) = unwrap_type(&ty);
                                        let mut ty = ty;
                                        if unwrapped {
                                            match ty.normalize() {
                                                Type::Tuple(Tuple { elems, .. }) => ty = *elems[idx].ty.clone(),
                                                _ => {}
                                            };
                                        }
                                        let type_params = self
                                            .analyzer
                                            .infer_ts_infer_types(span, &extends_type, &element.ty)
                                            .ok();
                                        if let Some(type_params) = type_params {
                                            ty = self.analyzer.expand_type_params(&type_params, ty).unwrap();
                                        }

                                        element.ty = box ty;
                                    }

                                    element
                                })
                                .collect();

                            return Type::Tuple(Tuple {
                                elems: elements,
                                ..obj_type.clone()
                            });
                        }
                        _ => {}
                    }

                    if let Some(v) = self
                        .analyzer
                        .extends(span, Default::default(), &obj_type, &extends_type)
                    {
                        let ty = if v { true_type } else { false_type };
                        let (_, mut ty) = unwrap_type(&**ty);

                        let type_params = self.analyzer.infer_ts_infer_types(span, &extends_type, &obj_type).ok();
                        if let Some(type_params) = type_params {
                            ty = self.analyzer.expand_type_params(&type_params, ty).unwrap();
                        }

                        return ty;
                    }

                    return ty;
                }

                Type::Union(Union { span, types }) => {
                    return Type::Union(Union { span, types });
                }

                Type::Function(ty::Function {
                    span,
                    type_params,
                    params,
                    ret_ty,
                }) => {
                    let ret_ty = self.analyzer.rename_type_params(span, *ret_ty, None)?;
                    let ret_ty = box ret_ty.foldable().fold_with(self);

                    return Type::Function(ty::Function {
                        span,
                        type_params,
                        params,
                        ret_ty,
                    });
                }

                ty => ty,
            }
        };

        let mut ty = match res {
            Ok(ty) => ty,
            Err(err) => {
                self.analyzer.storage.report(err);
                return Type::any(span);
            }
        };

        let _ctx = context(format!(
            "Expander.expand_type: {}",
            dump_type_as_string(&self.analyzer.cm, &ty)
        ));

        match ty {
            Type::Conditional(Conditional {
                span,
                mut check_type,
                mut extends_type,
                mut true_type,
                mut false_type,
                ..
            }) => {
                // We need to handle infer type.
                let type_params = self
                    .analyzer
                    .infer_ts_infer_types(self.span, &extends_type, &check_type)
                    .ok();

                if let Some(type_params) = type_params {
                    true_type = box self.analyzer.expand_type_params(&type_params, *true_type).unwrap();
                    false_type = box self.analyzer.expand_type_params(&type_params, *false_type).unwrap();
                }

                match check_type.normalize_mut() {
                    Type::Class(check_type) => match extends_type.normalize() {
                        Type::Constructor(..) => {
                            return *true_type;
                        }
                        _ => {}
                    },
                    _ => {}
                }

                return Type::Conditional(Conditional {
                    span,
                    check_type,
                    extends_type,
                    true_type,
                    false_type,
                });
            }
            _ => {}
        }

        ty
    }
}

impl Fold<ty::Function> for Expander<'_, '_, '_> {
    fn fold(&mut self, mut f: ty::Function) -> ty::Function {
        f.type_params = f.type_params.fold_with(self);
        f.params = f.params.fold_with(self);
        if self.analyzer.ctx.preserve_ret_ty {
            f.ret_ty = f.ret_ty.fold_with(self);
        }

        f
    }
}

impl Fold<ClassProperty> for Expander<'_, '_, '_> {
    fn fold(&mut self, value: ClassProperty) -> ClassProperty {
        value
    }
}

impl Fold<FnParam> for Expander<'_, '_, '_> {
    fn fold(&mut self, param: FnParam) -> FnParam {
        if self.analyzer.ctx.preserve_params || self.analyzer.is_builtin {
            return param;
        }

        param.fold_children_with(self)
    }
}

impl Fold<Type> for Expander<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        match ty {
            Type::Keyword(..) | Type::Lit(..) => return ty,
            Type::Arc(..) => {
                return ty.foldable().fold_with(self);
            }
            _ => {}
        }
        let before = dump_type_as_string(&self.analyzer.cm, &ty);
        let expanded = self.expand_type(ty).fixed();

        if !self.analyzer.is_builtin {
            expanded.assert_valid();
        }

        slog::debug!(
            self.logger,
            "[expander]: {} => {}",
            before,
            dump_type_as_string(&self.analyzer.cm, &expanded)
        );

        expanded
    }
}

impl Fold<stc_ts_types::Class> for Expander<'_, '_, '_> {
    fn fold(&mut self, node: stc_ts_types::Class) -> stc_ts_types::Class {
        node
    }
}

impl Fold<stc_ts_types::ClassMember> for Expander<'_, '_, '_> {
    fn fold(&mut self, node: stc_ts_types::ClassMember) -> stc_ts_types::ClassMember {
        node
    }
}

impl Fold<TypeElement> for Expander<'_, '_, '_> {
    fn fold(&mut self, node: TypeElement) -> TypeElement {
        node
    }
}

#[derive(Debug, Default)]
struct UnionFinder {
    found: bool,
}

impl Visit<PropertySignature> for UnionFinder {
    fn visit(&mut self, _: &PropertySignature) {}
}

impl Visit<ty::MethodSignature> for UnionFinder {
    fn visit(&mut self, _: &ty::MethodSignature) {}
}

impl Visit<Union> for UnionFinder {
    fn visit(&mut self, u: &Union) {
        self.found = true;
    }
}

pub(crate) struct ExpansionPreventer {
    mark: Mark,
}

impl VisitMut<Ref> for ExpansionPreventer {
    fn visit_mut(&mut self, ty: &mut Ref) {
        ty.span = ty.span.apply_mark(self.mark);
    }
}

impl VisitMut<Type> for ExpansionPreventer {
    fn visit_mut(&mut self, ty: &mut Type) {
        ty.normalize_mut();
        ty.visit_mut_children_with(self)
    }
}
