use super::{
    control_flow::CondFacts, expr::TypeOfMode, props::prop_name_to_expr,
    stmt::return_type::ReturnValues, Analyzer, Ctx,
};
use crate::{
    debug::print_backtrace,
    errors::Error,
    loader::ModuleInfo,
    name::Name,
    ty::{
        self, Alias, EnumVariant, IndexSignature, Interface, PropertySignature, Ref, Tuple, Type,
        TypeElement, TypeExt, TypeLit, Union,
    },
    type_facts::TypeFacts,
    util::{contains_infer_type, contains_mark, MarkFinder, RemoveTypes},
    validator::ValidateWith,
    ValidationResult,
};
use fxhash::{FxHashMap, FxHashSet};
use once_cell::sync::Lazy;
use rnode::Fold;
use rnode::FoldWith;
use rnode::Visit;
use rnode::VisitMut;
use rnode::VisitMutWith;
use rnode::VisitWith;
use slog::Logger;
use smallvec::SmallVec;
use stc_ts_ast_rnode::RArrayPat;
use stc_ts_ast_rnode::RAssignPatProp;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RKeyValuePatProp;
use stc_ts_ast_rnode::RObjectPat;
use stc_ts_ast_rnode::RObjectPatProp;
use stc_ts_ast_rnode::RPat;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RRestPat;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsKeywordType;
use stc_ts_ast_rnode::RTsQualifiedName;
use stc_ts_types::TypeParamInstantiation;
use stc_ts_types::{
    Conditional, FnParam, Id, IndexedAccessType, Mapped, ModuleId, Operator, QueryExpr, QueryType,
    StaticThis, TupleElement, TypeParam,
};
use stc_ts_utils::OptionExt;
use stc_ts_utils::PatExt;
use std::{borrow::Cow, collections::hash_map::Entry, fmt::Debug, iter, iter::repeat, slice};
use swc_atoms::js_word;
use swc_common::TypeEq;
use swc_common::{util::move_map::MoveMap, Mark, Span, Spanned, SyntaxContext, DUMMY_SP};
use swc_ecma_ast::*;

macro_rules! no_ref {
    ($t:expr) => {{
        match $t {
            Some(box Type::Ref(..)) => panic!("cannot store a variable with type `Ref`"),
            _ => {}
        }
    }};
}

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    logger: Logger,
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    pub declaring: SmallVec<[Id; 8]>,

    pub(super) vars: FxHashMap<Id, VarInfo>,
    pub(super) types: FxHashMap<Id, Vec<Box<Type>>>,
    pub(super) facts: CondFacts,

    pub(super) declaring_fn: Option<Id>,
    /// [Some] while declaring a class property or a property of an object
    /// literal.
    pub(super) declaring_prop: Option<Id>,

    pub(super) this: Option<Box<Type>>,
    /// Required to handle static properies.
    pub(super) this_class_name: Option<Id>,
    /// Only contains instance members.
    ///
    /// The value of [usize] should be ignored by methods except
    /// `Validate<Class>`
    pub(super) this_class_members: Vec<(usize, ty::ClassMember)>,

    pub(super) this_object_members: Vec<TypeElement>,

    pub(super) super_class: Option<Box<Type>>,

    pub(super) return_values: ReturnValues,

    /// `0` if we are not trying to expand it.
    expand_triage_depth: u8,

    /// Used to handle `...any` in calls.
    pub(super) is_call_arg_count_unknown: bool,
}

impl Scope<'_> {
    pub fn parent(&self) -> Option<&Self> {
        self.parent
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
            ScopeKind::Fn | ScopeKind::Method | ScopeKind::Class | ScopeKind::ObjectLit => {
                return true
            }
            _ => {}
        }

        self.parent
            .map(|scope| scope.is_this_defined())
            .unwrap_or(false)
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    /// Returns `true` if we are in call.
    pub fn is_calling(&self) -> bool {
        match self.kind {
            ScopeKind::Call => return true,
            _ => {}
        }

        match self.parent {
            Some(v) => v.is_calling(),
            None => false,
        }
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

    pub fn get_super_class(&self) -> Option<&Box<Type>> {
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
            expand_triage_depth: self.expand_triage_depth,
            return_values: self.return_values,
            is_call_arg_count_unknown: self.is_call_arg_count_unknown,
        }
    }

    pub fn copy_hoisted_vars_from(&mut self, from: &mut Scope) {
        match from.kind {
            // We don't copy variable information from nested function.
            ScopeKind::Method | ScopeKind::Fn | ScopeKind::ArrowFn => return,
            _ => {}
        }

        for (name, var) in from.vars.drain() {
            if var.kind == VarDeclKind::Var {
                self.vars.insert(name, var);
            }
        }
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

        self.vars.insert(name, v);
    }

    /// This method does **not** search for parent scope.
    pub fn get_var_mut(&mut self, name: &Id) -> Option<&mut VarInfo> {
        self.vars.get_mut(name)
    }

    /// Add a type to the scope.
    fn register_type(&mut self, name: Id, ty: Box<Type>) {
        let ty = ty.cheap();
        match ty.normalize() {
            Type::Param(..) => {
                // Override type parameter.
                // As if store type parameter two time, this is required.

                let v = self.types.entry(name).or_default();
                if let Some(index) = v.iter().position(|v| match &**v {
                    Type::Param(..) => true,
                    _ => false,
                }) {
                    v.remove(index);
                }
                v.push(ty);

                return;
            }
            _ => {}
        }
        self.types.entry(name).or_default().push(ty);
    }

    pub fn this(&self) -> Option<Cow<Box<Type>>> {
        if let Some(ref this) = self.this {
            return Some(Cow::Borrowed(this));
        }

        match self.parent {
            Some(ref parent) => parent.this(),
            None => None,
        }
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
    /// Overrides a variable. Used for removing lazily-typed stuffs.
    pub(super) fn override_var(
        &mut self,
        kind: VarDeclKind,
        name: Id,
        ty: Box<Type>,
    ) -> Result<(), Error> {
        self.declare_var(ty.span(), kind, name, Some(ty), true, true)?;

        Ok(())
    }

    /// Expands
    ///
    ///   - Type alias
    pub(super) fn expand(&mut self, span: Span, ty: Box<Type>) -> ValidationResult {
        if self.is_builtin {
            return Ok(ty);
        }

        let mut v = Expander {
            logger: self.logger.clone(),
            span,
            analyzer: self,
            dejavu: Default::default(),
            full: false,
            expand_union: false,
            expand_top_level: true,
        };
        Ok(box ty.foldable().fold_with(&mut v))
    }

    /// Expands
    ///
    /// // TODO: Add an option to expand union (this is required to assign)
    ///
    ///
    ///  - `expand_union` should be true if you are going to use it in
    ///    assignment, and false if you are going to use it in user-visible
    ///    stuffs (e.g. type annotation for .d.ts file)
    pub(super) fn expand_fully(
        &mut self,
        span: Span,
        ty: Box<Type>,
        expand_union: bool,
    ) -> ValidationResult {
        if self.is_builtin {
            return Ok(ty);
        }

        let mut v = Expander {
            logger: self.logger.clone(),
            span,
            analyzer: self,
            dejavu: Default::default(),
            full: true,
            expand_union,
            expand_top_level: true,
        };

        let ty = box ty.foldable().fold_with(&mut v);

        Ok(ty)
    }

    pub(super) fn register_type(&mut self, name: Id, ty: Box<Type>) -> Result<(), Error> {
        if self.ctx.in_global {
            self.env.declare_global_type(name.sym().clone(), ty.clone());
        }

        if self.is_builtin
            && match ty.normalize() {
                Type::EnumVariant(_)
                | Type::Interface(_)
                | Type::Enum(_)
                | Type::Mapped(_)
                | Type::Alias(_)
                | Type::Namespace(_)
                | Type::Module(_)
                | Type::Class(_)
                | Type::ClassInstance(_)
                | Type::Intersection(_)
                | Type::Function(_)
                | Type::Constructor(_)
                | Type::Union(_)
                | Type::Array(_)
                | Type::Tuple(_)
                | Type::Keyword(_)
                | Type::Conditional(_)
                | Type::TypeLit(_)
                | Type::Ref(_)
                | Type::IndexedAccessType(_)
                | Type::Import(_)
                | Type::Query(_)
                | Type::Lit(_)
                | Type::This(_) => true,

                _ => false,
            }
        {
            self.storage
                .store_private_type(ModuleId::builtin(), name, ty.cheap());
        } else {
            slog::trace!(self.logger, "register_type({})", name);
            let ty = ty.cheap();

            if self.scope.is_root() {
                self.storage
                    .store_private_type(self.ctx.module_id, name.clone(), ty.clone());
            }

            self.scope.register_type(name, ty);
        }

        Ok(())
    }

    pub fn declare_vars(&mut self, kind: VarDeclKind, pat: &RPat) -> Result<(), Error> {
        self.declare_vars_inner_with_ty(kind, pat, false, None)
    }

    pub fn declare_vars_with_ty(
        &mut self,
        kind: VarDeclKind,
        pat: &RPat,
        ty: Option<Box<Type>>,
    ) -> Result<(), Error> {
        self.declare_vars_inner_with_ty(kind, pat, false, ty)
    }

    pub(super) fn declare_vars_inner(
        &mut self,
        kind: VarDeclKind,
        pat: &RPat,
        export: bool,
    ) -> Result<(), Error> {
        self.declare_vars_inner_with_ty(kind, pat, export, None)
    }

    /// Updates variable list.
    ///
    /// This method should be called for function parameters including error
    /// variable from a catch clause.
    fn declare_vars_inner_with_ty(
        &mut self,
        kind: VarDeclKind,
        pat: &RPat,
        export: bool,
        ty: Option<Box<Type>>,
    ) -> Result<(), Error> {
        let span = ty
            .as_ref()
            .map(|v| v.span())
            .and_then(|span| if span.is_dummy() { None } else { Some(span) })
            .unwrap_or_else(|| pat.span());
        if !self.is_builtin {
            assert_ne!(span, DUMMY_SP);
        }

        match &*pat {
            RPat::Ident(i) => {
                let name: Id = Id::from(i.clone());
                if !self.is_builtin {
                    debug_assert_ne!(span, DUMMY_SP);
                }
                let ty = match ty {
                    None => try_opt!(i.type_ann.as_ref().map(|v| v.type_ann.validate_with(self))),
                    Some(ty) => Some(ty),
                };
                self.declare_var(
                    span,
                    kind,
                    name.clone(),
                    ty.clone(),
                    // initialized
                    true,
                    // allow_multiple
                    kind == VarDeclKind::Var,
                )?;
                if export {
                    self.storage.store_private_var(
                        self.ctx.module_id,
                        name.clone(),
                        ty.unwrap_or(Type::any(i.span)),
                    );
                    self.storage.export_var(span, self.ctx.module_id, name);
                }
                return Ok(());
            }
            RPat::Assign(ref p) => {
                let ty = match ty {
                    Some(ty) => ty,
                    None => p.right.validate_with_default(self)?,
                };
                slog::debug!(
                    self.logger,
                    "({}) declare_vars: Assign({:?}), ty = {:?}",
                    self.scope.depth(),
                    p.left,
                    ty
                );
                self.declare_vars_inner_with_ty(kind, &p.left, export, Some(ty))?;

                return Ok(());
            }

            RPat::Array(RArrayPat {
                span,
                ref elems,
                ref type_ann,
                ref optional,
                node_id,
                ..
            }) => {
                // TODO: Handle type annotation

                if type_ann.is_none() {
                    if let Some(ty) = ty {
                        if let Some(m) = &mut self.mutations {
                            m.for_pats.entry(*node_id).or_default().optional = Some(true);
                            m.for_pats
                                .entry(*node_id)
                                .or_default()
                                .ty
                                .fill_with(|| ty.generalize_lit().generalize_tuple().into());
                        }
                    }
                }

                if type_ann.is_none() {
                    let ctxt = self.ctx.module_id;
                    if let Some(m) = &mut self.mutations {
                        //
                        m.for_pats.entry(*node_id).or_default().ty.fill_with(|| {
                            box Type::Ref(Ref {
                                span: *span,
                                ctxt,
                                type_name: RTsEntityName::Ident(RIdent::new(
                                    "Iterable".into(),
                                    DUMMY_SP,
                                )),
                                type_args: Some(TypeParamInstantiation {
                                    span: *span,
                                    params: vec![box Type::Keyword(RTsKeywordType {
                                        span: DUMMY_SP,
                                        kind: TsKeywordTypeKind::TsAnyKeyword,
                                    })],
                                }),
                            })
                        });
                    }
                }

                for elem in elems.iter() {
                    match *elem {
                        Some(ref elem) => {
                            self.declare_vars_inner(kind, elem, export)?;
                        }
                        // Skip
                        None => {}
                    }
                }

                return Ok(());
            }

            RPat::Object(RObjectPat {
                ref props,
                ref type_ann,
                node_id,
                ..
            }) => {
                if type_ann.is_none() {
                    if let Some(m) = &mut self.mutations {
                        m.for_pats.entry(*node_id).or_default().ty =
                            Some(box Type::TypeLit(TypeLit {
                                span,
                                // TODO: Fill it
                                members: vec![],
                            }));
                    }
                }
                for prop in props {
                    match *prop {
                        RObjectPatProp::KeyValue(RKeyValuePatProp { .. }) => {
                            unimplemented!("ket value pattern in object pattern")
                        }
                        RObjectPatProp::Assign(RAssignPatProp { .. }) => {
                            unimplemented!("assign pattern in object pattern")
                        }
                        RObjectPatProp::Rest(RRestPat { .. }) => {
                            unimplemented!("rest pattern in object pattern")
                        }
                    }
                }

                return Ok(());
            }

            RPat::Rest(RRestPat {
                ref arg,
                type_ann: ref ty,
                node_id,
                ..
            }) => {
                let mut arg = arg.clone();

                self.declare_vars_inner(kind, &arg, export)?;

                let new_ty = arg.get_mut_ty().take();
                if ty.is_none() {
                    if let Some(arg_node_id) = arg.node_id() {
                        if let Some(m) = &mut self.mutations {
                            let ty = m.for_pats.entry(arg_node_id).or_default().ty.take();
                            if let Some(ty) = ty {
                                m.for_pats.entry(*node_id).or_default().ty = Some(ty);
                            }
                        }
                    }
                }

                return Ok(());
            }

            RPat::Invalid(..) | RPat::Expr(box RExpr::Invalid(..)) => Ok(()),

            _ => unimplemented!("declare_vars for patterns other than ident: {:#?}", pat),
        }
    }

    #[inline(never)]
    pub(super) fn find_var(&self, name: &Id) -> Option<&VarInfo> {
        static ANY_VAR: Lazy<VarInfo> = Lazy::new(|| VarInfo {
            ty: Some(Type::any(DUMMY_SP)),
            kind: VarDeclKind::Const,
            initialized: true,
            copied: false,
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

    pub(super) fn find_var_type(&self, name: &Id) -> Option<Cow<Box<Type>>> {
        if let Some(v) = self.cur_facts.true_facts.vars.get(&Name::from(name)) {
            return Some(Cow::Borrowed(v));
        }

        // println!("({}) find_var_type({})", self.scope.depth(), name);
        let mut scope = Some(&self.scope);
        while let Some(s) = scope {
            if let Some(ref v) = s.facts.vars.get(&Name::from(name)) {
                slog::debug!(
                    self.logger,
                    "Scope.find_var_type({}): Handled from facts",
                    name
                );
                return Some(Cow::Borrowed(v));
            }

            scope = s.parent;
        }

        {
            if let Some(ty) = self.storage.get_local_var(self.ctx.module_id, name.clone()) {
                return Some(Cow::Owned(ty));
            }
        }

        {
            // Improted variables
            if let Some(info) = self.imports_by_id.get(name) {
                if let Some(var_ty) = info.data.vars.get(name) {
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

            let mut ty = match var.ty {
                Some(ref ty) => ty.clone(),
                _ => return None,
            };

            if let Some(ref excludes) = self.scope.facts.excludes.get(&name) {
                match *ty {
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
            }

            return Some(Cow::Owned(ty));
        }

        None
    }

    pub fn find_type(
        &self,
        target: ModuleId,
        name: &Id,
    ) -> ValidationResult<Option<ItemRef<Type>>> {
        if target == self.ctx.module_id || target.is_builtin() {
            if let Some(v) = self.find_local_type(name) {
                return Ok(Some(v));
            }
        }

        if let Some(ModuleInfo { data, .. }) = self.imports_by_id.get(name) {
            if let Some(types) = data.types.get(name) {
                let types = types.clone();
                return Ok(Some(ItemRef::Owned(types.into_iter())));
            }
        }

        if let Ok(ty) = self.env.get_global_type(DUMMY_SP, &name.sym()) {
            return Ok(Some(ItemRef::Owned(vec![ty].into_iter())));
        }

        if let Some(data) = self.imports.get(&(self.ctx.module_id, target)) {
            if let Some(types) = data.types.get(name) {
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

        slog::debug!(
            self.logger,
            "({}) Analyzer.find_type(`{}`)",
            self.scope.depth(),
            name
        );

        if !self.is_builtin {
            if let Ok(ty) = self.env.get_global_type(DUMMY_SP, name.sym()) {
                debug_assert!(ty.is_clone_cheap(), "{:?}", ty);

                slog::debug!(self.logger, "Using builtin / global type");
                return Some(ItemRef::Owned(vec![ty].into_iter()));
            }
        }

        if let Some(ty) = self.scope.find_type(name) {
            slog::debug!(self.logger, "Using type from scope: {:?}", ty);
            return Some(ty);
        }

        if let Some(ty) = self
            .storage
            .get_local_type(self.ctx.module_id, name.clone())
        {
            return Some(ItemRef::Owned(vec![ty].into_iter()));
        }

        if !self.is_builtin {
            slog::debug!(
                self.logger,
                "Scope.find_type: failed to find type '{}'",
                name
            );
        }

        None
    }

    pub(super) fn mark_var_as_truthy(&mut self, name: Id) -> ValidationResult<()> {
        self.modify_var(name, |var| {
            var.ty = var.ty.take().map(|ty| box ty.remove_falsy());
            Ok(())
        })
    }

    fn modify_var<F, Ret>(&mut self, name: Id, op: F) -> ValidationResult<Ret>
    where
        F: FnOnce(&mut VarInfo) -> ValidationResult<Ret>,
    {
        let var = self.find_var(&name);
        let ty = var.map(|var| var.ty.clone()).flatten();

        op(self.scope.vars.entry(name).or_insert_with(|| VarInfo {
            kind: VarDeclKind::Let,
            initialized: true,
            ty,
            copied: true,
        }))
    }

    pub fn declare_var(
        &mut self,
        span: Span,
        kind: VarDeclKind,
        name: Id,
        ty: Option<Box<Type>>,
        initialized: bool,
        allow_multiple: bool,
    ) -> Result<(), Error> {
        if self.ctx.in_global {
            if let Some(ty) = ty.clone() {
                self.env.declare_global_var(name.sym().clone(), ty.clone());
            }
        }

        if self.scope.is_root() {
            if let Some(ty) = ty.clone() {
                self.storage
                    .store_private_var(self.ctx.module_id, name.clone(), ty)
            }
        }

        match self.scope.vars.entry(name.clone()) {
            Entry::Occupied(e) => {
                if !allow_multiple {
                    print_backtrace();
                    panic!("{:?}", Error::DuplicateName { name, span });
                }
                //println!("\tdeclare_var: found entry");
                let (k, mut v) = e.remove_entry();

                macro_rules! restore {
                    () => {{
                        self.scope.vars.insert(k, v);
                    }};
                }

                v.ty = if let Some(ty) = ty {
                    Some(if let Some(var_ty) = v.ty {
                        match ty.normalize() {
                            Type::Union(..) => {
                                // TODO: Check if all types are query or
                                // function
                            }
                            Type::Query(..) | Type::Function(..) => {}
                            _ => {
                                let generalized_var_ty = var_ty.clone().generalize_lit();

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
                                        let var_ty =
                                            self.expand_fully(span, generalized_var_ty, true)?;

                                        let res = self.assign(&ty, &var_ty, span);

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
                        Type::union(vec![var_ty, ty])
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

                self.scope.vars.insert(k, v);
            }
            Entry::Vacant(e) => {
                //println!("\tdeclare_var: no entry");

                let info = VarInfo {
                    kind,
                    ty,
                    initialized,
                    copied: false,
                };
                e.insert(info);
            }
        }

        Ok(())
    }

    pub fn declare_complex_vars(
        &mut self,
        kind: VarDeclKind,
        pat: &RPat,
        ty: Box<Type>,
    ) -> ValidationResult<()> {
        let span = pat.span();

        match *pat {
            RPat::Ident(ref i) => {
                slog::debug!(&self.logger, "declare_complex_vars: declaring {}", i.sym);
                self.declare_var(
                    span,
                    kind,
                    i.into(),
                    Some(ty),
                    // initialized
                    true,
                    // let/const declarations does not allow multiple declarations with
                    // same name
                    kind == VarDeclKind::Var,
                )?;
                Ok(())
            }

            RPat::Array(RArrayPat { ref elems, .. }) => {
                // Handle tuple
                //
                //      const [a , setA] = useState();
                //

                let ty = box ty.foldable();
                match *ty {
                    Type::Tuple(Tuple {
                        elems: ref tuple_elements,
                        ..
                    }) => {
                        if tuple_elements.len() < elems.len() {
                            return Err(Error::TooManyTupleElements { span });
                        }

                        for (elem, tuple_element) in elems.into_iter().zip(tuple_elements) {
                            match *elem {
                                Some(ref elem) => {
                                    self.declare_complex_vars(
                                        kind,
                                        elem,
                                        tuple_element.ty.clone(),
                                    )?;
                                }
                                None => {
                                    // Skip
                                }
                            }
                        }

                        return Ok(());
                    }

                    // [a, b] | [c, d] => [a | c, b | d]
                    Type::Union(Union { types, .. }) => {
                        let mut errors = vec![];
                        let mut buf = vec![];
                        for ty in types.iter() {
                            match *ty.normalize() {
                                Type::Tuple(Tuple {
                                    elems: ref elem_types,
                                    ..
                                }) => {
                                    buf.push(elem_types);
                                }
                                _ => {
                                    errors.push(Error::NotTuple { span: ty.span() });
                                }
                            }
                        }
                        if !errors.is_empty() {
                            return Err(Error::UnionError { span, errors });
                        }

                        for (elem, tuple_elements) in
                            elems.into_iter().zip(buf.into_iter().chain(repeat(&vec![
                                TupleElement {
                                    span: DUMMY_SP,
                                    label: None,
                                    ty: Type::undefined(span),
                                },
                            ])))
                        {
                            match *elem {
                                Some(ref elem) => {
                                    let ty = box Union {
                                        span,
                                        types: tuple_elements
                                            .into_iter()
                                            .map(|element| &element.ty)
                                            .cloned()
                                            .collect(),
                                    }
                                    .into();
                                    self.declare_complex_vars(kind, elem, ty)?;
                                }
                                None => {}
                            }
                        }

                        return Ok(());
                    }

                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        // Everything is any
                        for elem in elems {
                            match elem {
                                Some(elem) => {
                                    self.declare_complex_vars(kind, elem, ty.clone())?;
                                }
                                None => {}
                            }
                        }

                        return Ok(());
                    }

                    _ => unimplemented!("declare_complex_vars(pat={:?}\nty={:?}\n)", pat, ty),
                }
            }

            RPat::Object(RObjectPat { ref props, .. }) => {
                fn find<'a>(members: &[TypeElement], key: &RPropName) -> Option<Box<Type>> {
                    let mut index_el = None;
                    // First, we search for Property
                    for m in members {
                        match *m {
                            TypeElement::Property(PropertySignature { ref type_ann, .. }) => {
                                return match *type_ann {
                                    Some(ref ty) => Some(ty.clone()),
                                    None => Some(Type::any(key.span())),
                                }
                            }

                            TypeElement::Index(IndexSignature { ref type_ann, .. }) => {
                                index_el = Some(match *type_ann {
                                    Some(ref ty) => ty.clone(),
                                    None => Type::any(key.span()),
                                });
                            }
                            _ => {}
                        }
                    }

                    return index_el;
                }

                /// Handle TypeElements.
                ///
                /// Used for interfaces and type literals.
                fn handle_elems(
                    a: &mut Analyzer,
                    kind: VarDeclKind,
                    span: Span,
                    props: &[RObjectPatProp],
                    members: &[TypeElement],
                ) -> ValidationResult<()> {
                    for p in props.iter() {
                        match *p {
                            RObjectPatProp::KeyValue(RKeyValuePatProp {
                                ref key,
                                ref value,
                                ..
                            }) => {
                                if let Some(ty) = find(&members, key) {
                                    a.declare_complex_vars(kind, value, ty)?;
                                    return Ok(());
                                }
                            }

                            RObjectPatProp::Assign(RAssignPatProp {
                                ref key,
                                value: None,
                                ..
                            }) => {
                                if let Some(ty) = find(&members, &RPropName::Ident(key.clone())) {
                                    a.declare_complex_vars(kind, &RPat::Ident(key.clone()), ty)?;
                                    return Ok(());
                                }
                            }

                            _ => unimplemented!("handle_elems({:#?})", p),
                        }
                    }

                    dbg!();

                    return Err(Error::NoSuchProperty {
                        span,
                        obj: None,
                        prop: None,
                        prop_ty: None,
                    });
                }

                // TODO: Normalize static
                match ty.normalize() {
                    Type::TypeLit(TypeLit { members, .. }) => {
                        return handle_elems(self, kind, span, props, &members);
                    }

                    // TODO: Handle extends
                    Type::Interface(Interface { body, .. }) => {
                        return handle_elems(self, kind, span, props, &body);
                    }

                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        for p in props.iter() {
                            match p {
                                RObjectPatProp::KeyValue(ref kv) => {
                                    self.declare_complex_vars(
                                        kind,
                                        &kv.value,
                                        Type::any(kv.span()),
                                    )?;
                                }

                                RObjectPatProp::Assign(RAssignPatProp {
                                    span,
                                    key,
                                    value: None,
                                    ..
                                }) => {
                                    self.declare_complex_vars(
                                        kind,
                                        &RPat::Ident(key.clone()),
                                        Type::any(*span),
                                    )?;
                                }

                                _ => unimplemented!("handle_elems({:#?})", p),
                            }
                        }

                        return Ok(());
                    }

                    Type::Keyword(RTsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        // TODO: Somehow get precise logic of determining span.
                        //
                        // let { ...a } = x;
                        //          ^
                        //

                        // WTF...
                        for p in props.iter().rev() {
                            let span = match p {
                                RObjectPatProp::Rest(RRestPat { ref arg, .. }) => arg.span(),
                                _ => p.span(),
                            };
                            return Err(Error::Unknown { span });
                        }

                        return Err(Error::Unknown { span });
                    }

                    Type::Ref(..) => {
                        let ctx = Ctx {
                            preserve_ref: false,
                            ignore_expand_prevention_for_top: true,
                            ..self.ctx
                        };
                        let ty = self.with_ctx(ctx).expand_fully(span, ty, false)?;

                        return self.declare_complex_vars(kind, pat, ty);
                    }

                    // TODO: Check if allowing class is same (I mean static vs instance method
                    // issue)
                    Type::Class(..) | Type::Array(..) | Type::This(..) => {
                        //

                        for prop in props {
                            match prop {
                                RObjectPatProp::KeyValue(prop) => {
                                    let mut key_expr = prop_name_to_expr(&prop.key);

                                    let ty = self.access_property(
                                        span,
                                        ty.clone(),
                                        &mut key_expr,
                                        false,
                                        TypeOfMode::RValue,
                                    )?;

                                    self.declare_complex_vars(kind, &prop.value, ty)?;
                                }
                                RObjectPatProp::Assign(prop) => {
                                    let mut key_expr = box RExpr::Ident(prop.key.clone());

                                    let ty = self.access_property(
                                        span,
                                        ty.clone(),
                                        &mut key_expr,
                                        false,
                                        TypeOfMode::RValue,
                                    )?;

                                    match prop.value {
                                        Some(_) => {
                                            unimplemented!("pattern with default where rhs is this")
                                        }
                                        None => {
                                            self.declare_complex_vars(
                                                kind,
                                                &RPat::Ident(prop.key.clone()),
                                                ty,
                                            )?;
                                        }
                                    }
                                }
                                RObjectPatProp::Rest(_) => {
                                    unimplemented!("rest pattern where rhs is this")
                                }
                            }
                        }

                        return Ok(());
                    }

                    _ => {
                        print_backtrace();

                        unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty)
                    }
                }
            }

            _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
        }
    }

    pub(crate) fn contains_infer_type<T>(&self, ty: &T) -> bool
    where
        T: VisitWith<MarkFinder>,
    {
        contains_mark(ty, self.marks().contains_infer_type_mark)
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
    pub kind: VarDeclKind,
    pub initialized: bool,
    pub ty: Option<Box<Type>>,
    /// Copied from parent scope. If this is true, it's not a variable
    /// declaration.
    pub copied: bool,
}

impl<'a> Scope<'a> {
    pub const fn kind(&self) -> ScopeKind {
        self.kind
    }

    /// Returns true if `this` (from javascript) is a reference to an object
    /// literal.
    pub fn is_this_ref_to_object_lit(&self) -> bool {
        match self.kind {
            ScopeKind::Fn | ScopeKind::ArrowFn => return false,

            // `this` in object literal resolves to the object literal itself.
            ScopeKind::ObjectLit => return true,

            ScopeKind::Class => return false,
            ScopeKind::Call | ScopeKind::Method | ScopeKind::Flow | ScopeKind::Block => {}
        }

        match self.parent {
            Some(parent) => parent.is_this_ref_to_object_lit(),
            None => false,
        }
    }

    /// Returns true if `this` (from javascript) is a reference to a class.
    pub fn is_this_ref_to_class(&self) -> bool {
        match self.kind {
            ScopeKind::Fn | ScopeKind::ArrowFn => return false,

            // `this` in object literal resolves to the object literal itself.
            ScopeKind::ObjectLit => return false,

            ScopeKind::Class => return true,
            ScopeKind::Call | ScopeKind::Method | ScopeKind::Flow | ScopeKind::Block => {}
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

    fn new_inner(
        logger: Logger,
        parent: Option<&'a Scope<'a>>,
        kind: ScopeKind,
        facts: CondFacts,
    ) -> Self {
        Scope {
            logger,
            parent,

            kind,
            declaring: Default::default(),
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
            expand_triage_depth: 0,
            return_values: Default::default(),
            is_call_arg_count_unknown: false,
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
            for ty in &**ty {
                debug_assert!(ty.is_clone_cheap(), "{:?}", ty);
            }

            // println!("({}) find_type({}): Found", self.depth(), name);

            return Some(ItemRef::Multi(ty.iter()));
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
    Boxed(iter::Once<&'a Box<T>>),
    Multi(slice::Iter<'a, Box<T>>),
    Owned(std::vec::IntoIter<Box<T>>),
}

impl<'a, T> Iterator for ItemRef<'a, T>
where
    T: Clone,
{
    type Item = Cow<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self {
            ItemRef::Single(v) => Cow::Borrowed(v.next()?),
            ItemRef::Boxed(v) => Cow::Borrowed(v.next().map(|v| &**v)?),
            ItemRef::Multi(v) => Cow::Borrowed(v.next().map(|v| &**v)?),
            ItemRef::Owned(v) => Cow::Owned(v.next().map(|v| *v)?),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeKind {
    Block,
    Fn,
    /// This does not affect `this`.
    Method,
    ArrowFn,
    /// This variant is related to handling of `this.foo()` in class methods.
    Class,
    /// This variant is related to handling of `this.foo()` in method
    /// properties.
    ObjectLit,
    /// If statement, conditional expression, switch case
    Flow,
    /// Type parameters are stored in this scope.
    Call,
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
    fn expand_ref(&mut self, r: Ref, was_top_level: bool) -> ValidationResult<Option<Type>> {
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

        match type_name {
            RTsEntityName::Ident(ref i) => {
                if let Some(class) = &self.analyzer.scope.get_this_class_name() {
                    if *class == *i {
                        return Ok(None);
                    }
                }

                slog::info!(self.logger, "Info: {}{:?}", i.sym, i.span.ctxt);
                if !trying_primitive_expansion && self.dejavu.contains(&i.into()) {
                    slog::error!(self.logger, "Dejvu: {}{:?}", &i.sym, i.span.ctxt);
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
                        let is_class = match t.normalize() {
                            Type::Class(..) => true,
                            _ => false,
                        };

                        match t.normalize() {
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
                            | Type::Class(ty::Class { type_params, .. }) => {
                                let ty = t.clone().into_owned();
                                let type_params = type_params.clone();
                                let type_args: Option<_> = type_args.clone().fold_with(self);

                                if let Some(type_params) = type_params {
                                    slog::info!(self.logger, "expand: expanding type parameters");
                                    let mut inferred = self.analyzer.infer_arg_types(
                                        self.span,
                                        type_args.as_ref(),
                                        &type_params.params,
                                        &[],
                                        &[],
                                        &Type::TypeLit(TypeLit {
                                            span,
                                            members: vec![],
                                        }),
                                    )?;
                                    inferred.iter_mut().for_each(|(_, ty)| {
                                        self.analyzer.allow_expansion(&mut **ty);
                                    });

                                    let mut ty = *self
                                        .analyzer
                                        .expand_type_params(&inferred, box ty.foldable())?;

                                    if !is_class {
                                        self.dejavu.insert(i.into());
                                        ty = ty.fold_with(self);
                                        self.dejavu.remove(&i.into());
                                    }

                                    return Ok(Some(ty));
                                }

                                match ty.normalize() {
                                    Type::Interface(..)
                                        if !trying_primitive_expansion && was_top_level =>
                                    {
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

                                if !is_class {
                                    self.dejavu.insert(i.into());
                                    ty = ty.fold_with(self);
                                    self.dejavu.remove(&i.into());
                                }
                                return Ok(Some(ty));
                            }

                            Type::Mapped(m) => {}

                            _ => stored_ref = Some(t),
                        }
                    }

                    if let Some(t) = stored_ref {
                        self.expand_top_level = true;
                        return Ok(Some(t.into_owned().foldable().fold_with(self)));
                    }
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
            RTsEntityName::TsQualifiedName(box RTsQualifiedName {
                left: RTsEntityName::Ident(ref left),
                ref right,
                ..
            }) => {
                if left.sym == js_word!("void") {
                    return Ok(Some(*Type::any(span)));
                }

                if let Some(types) = self.analyzer.find_type(ctxt, &left.into())? {
                    for ty in types {
                        match ty.normalize() {
                            Type::Enum(..) => {
                                return Ok(Some(
                                    EnumVariant {
                                        span,
                                        ctxt: self.analyzer.ctx.module_id,
                                        enum_name: left.into(),
                                        name: right.sym.clone(),
                                    }
                                    .into(),
                                ));
                            }
                            Type::Param(..) | Type::Namespace(..) | Type::Module(..) => {
                                return Ok(Some(ty.into_owned()))
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {
                unimplemented!("TsEntityName: {:?}", type_name);
            }
        }

        Err(Error::NameNotFound {
            name: type_name.clone().into(),
            ctxt,
            type_args: type_args.clone(),
            span,
        })
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

impl Fold<FnParam> for Expander<'_, '_, '_> {
    fn fold(&mut self, param: FnParam) -> FnParam {
        if self.analyzer.ctx.preserve_params {
            return param;
        }

        param.fold_children_with(self)
    }
}

impl Fold<Type> for Expander<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        if self.analyzer.is_builtin {
            return ty;
        }

        match ty {
            Type::Keyword(..) | Type::Lit(..) => return ty,
            Type::Arc(..) => {
                return ty.foldable().fold_with(self);
            }
            _ => {}
        }

        slog::debug!(self.logger, "Expanding type: {:?}", ty);

        self.full |= match ty {
            Type::Mapped(..) => true,
            _ => false,
        };

        let trying_primitive_expansion = self.analyzer.scope.expand_triage_depth != 0;

        // If we are trying to expand types as primitive, we ignore config
        let is_expansion_prevented =
            !trying_primitive_expansion && self.analyzer.is_expansion_prevented(&ty);

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
                                expr: QueryExpr::TsEntityName(RTsEntityName::Ident(name)),
                                ..
                            }) => {
                                let id = (&*name).into();
                                let ctxt = self.analyzer.ctx.module_id;
                                //
                                if let Some(ty) = self.analyzer.find_var_type(&id) {
                                    *cond_ty.check_type = *ty.into_owned();
                                } else {
                                    slog::error!(
                                        self.analyzer.logger,
                                        "Failed to find variable named {:?}",
                                        id
                                    );
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
                                        self.analyzer.extends(&element.ty, &extends_type)
                                    {
                                        let ty = if v { true_type } else { false_type };

                                        let (unwrapped, ty) = unwrap_type(&ty);
                                        let mut ty = box ty;
                                        if unwrapped {
                                            match *ty {
                                                Type::Tuple(Tuple { elems, .. }) => {
                                                    ty = elems[idx].ty.clone()
                                                }
                                                _ => {}
                                            };
                                        }
                                        let type_params = self
                                            .analyzer
                                            .infer_ts_infer_types(&extends_type, &element.ty)
                                            .ok();
                                        if let Some(type_params) = type_params {
                                            ty = self
                                                .analyzer
                                                .expand_type_params(&type_params, ty)
                                                .unwrap();
                                        }

                                        element.ty = ty;
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

                    if let Some(v) = self.analyzer.extends(&obj_type, &extends_type) {
                        let ty = if v { true_type } else { false_type };
                        let (_, mut ty) = unwrap_type(&**ty);

                        let type_params = self
                            .analyzer
                            .infer_ts_infer_types(&extends_type, &obj_type)
                            .ok();
                        if let Some(type_params) = type_params {
                            ty = *self
                                .analyzer
                                .expand_type_params(&type_params, box ty)
                                .unwrap();
                        }

                        return ty;
                    }

                    return ty;
                }

                Type::Interface(i) => {
                    // TODO: Handle type params
                    return Type::TypeLit(TypeLit {
                        span,
                        members: i.body,
                    });
                }

                Type::Union(Union { span, types }) => {
                    return *Type::union(types);
                }

                Type::Function(ty::Function {
                    span,
                    type_params,
                    params,
                    ret_ty,
                }) => {
                    let ret_ty = self.analyzer.rename_type_params(span, ret_ty, None)?;
                    let ret_ty = box ret_ty.foldable().fold_with(self);

                    return Type::Function(ty::Function {
                        span,
                        type_params,
                        params,
                        ret_ty,
                    });
                }

                ty => box ty,
            }
        };

        let mut ty = match res {
            Ok(ty) => *ty,
            Err(err) => {
                self.analyzer.storage.report(err);
                return *Type::any(span);
            }
        };

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
                    .infer_ts_infer_types(&extends_type, &check_type)
                    .ok();

                if let Some(type_params) = type_params {
                    true_type = self
                        .analyzer
                        .expand_type_params(&type_params, true_type)
                        .unwrap();
                    false_type = self
                        .analyzer
                        .expand_type_params(&type_params, false_type)
                        .unwrap();
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
