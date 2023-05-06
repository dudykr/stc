use std::{
    cell::Cell,
    fmt::Debug,
    mem::take,
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
};

use fxhash::{FxHashMap, FxHashSet};
use rnode::VisitWith;
use stc_ts_ast_rnode::{
    RDecorator, RModule, RModuleDecl, RModuleItem, RScript, RStmt, RStr, RTsImportEqualsDecl, RTsModuleBlock, RTsModuleDecl, RTsModuleName,
    RTsModuleRef, RTsNamespaceDecl,
};
use stc_ts_base_type_ops::bindings::Bindings;
use stc_ts_dts_mutations::Mutations;
use stc_ts_env::{Env, Marks, ModuleConfig, Rule, StableEnv};
use stc_ts_errors::{debug::debugger::Debugger, DebugExt, ErrorKind};
use stc_ts_storage::{Builtin, Info, Storage};
use stc_ts_type_cache::TypeCache;
use stc_ts_types::{type_id::DestructureId, Id, IdCtx, Key, ModuleId, ModuleTypeData, Namespace};
use stc_ts_utils::StcComments;
use stc_utils::{cache::Freeze, FxHashMap, FxHashSet};
use swc_atoms::{js_word, JsWord};
use swc_common::{FileName, SourceMap, Span, DUMMY_SP, GLOBALS};
use swc_ecma_ast::*;

use self::{
    control_flow::{CondFacts, Facts},
    pat::PatMode,
    props::ComputedPropMode,
    scope::{Scope, VarKind},
    util::ResultExt,
};
pub(crate) use self::{scope::ScopeKind, types::NormalizeTypeOpts};
use crate::{
    loader::{Load, ModuleInfo},
    ty,
    ty::Type,
    validator,
    validator::ValidateWith,
    VResult,
};

macro_rules! try_opt {
    ($e:expr) => {{
        match $e {
            Some(v) => Some(v?),
            None => None,
        }
    }};
}

mod assign;
mod class;
mod control_flow;
mod convert;
mod decl_merging;
mod enums;
mod export;
mod expr;
mod function;
mod generalize;
mod generic;
mod hoisting;
mod import;
mod pat;
mod props;
mod relation;
mod scope;
mod stmt;
#[cfg(test)]
mod tests;
mod tsc_helper;
mod types;
mod util;
mod visit_mut;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ctx {
    module_id: ModuleId,

    /// `true` for the **body** of class members. This is false for class keys
    /// of a non-nested class declaration.
    in_class_member: bool,

    in_const_assertion: bool,

    in_constructor_param: bool,

    disallow_unknown_object_property: bool,

    use_undefined_for_empty_array_lit: bool,

    allow_module_var: bool,

    check_for_implicit_any: bool,

    /// If `true`, expression validator will not emit tuple.
    array_lit_cannot_be_tuple: bool,
    prefer_tuple_for_array_lit: bool,

    in_shorthand: bool,

    /// Used to make type parameters `unknown` when it cannot be inferred.
    is_instantiating_class: bool,

    /// `true` for condition of conditional expression or of an if statement.
    in_cond: bool,
    should_store_truthy_for_access: bool,
    in_switch_case_test: bool,

    in_computed_prop_name: bool,

    in_opt_chain: bool,

    in_declare: bool,
    in_fn_without_body: bool,
    in_global: bool,
    in_export_assignment: bool,
    in_export_default_expr: bool,

    in_async: bool,
    in_generator: bool,

    is_calling_iife: bool,

    in_useless_expr_for_seq: bool,

    in_ts_fn_type: bool,

    /// `true` if unresolved references should be reported.
    ///
    /// For example, while validating type parameter instantiation, unresolved
    /// references are error.
    in_actual_type: bool,

    /// If true, `type_of_raw_var` should report an error if the referenced
    /// variable is global.
    report_error_for_non_local_vars: bool,

    in_static_property_initializer: bool,
    in_static_block: bool,
    in_static_method: bool,

    reevaluating_call_or_new: bool,
    reevaluating_argument: bool,

    /// If true, all errors should be ignored.
    ///
    /// Used to prevent wrong errors while validating loop bodies or etc.
    ignore_errors: bool,

    var_kind: VarDeclKind,
    pat_mode: PatMode,
    computed_prop_mode: ComputedPropMode,
    allow_ref_declaring: bool,
    in_argument: bool,
    in_fn_with_return_type: bool,
    in_return_arg: bool,
    in_assign_rhs: bool,

    in_export_decl: bool,
    in_export_named: bool,
    skip_identical_while_inference: bool,

    super_references_super_class: bool,

    /// true if the `ClassDef` has the super class
    in_class_with_super: bool,

    /// `generatorReturnTypeFallback.3.ts` says
    ///
    /// Do not allow generators to fallback to IterableIterator while in
    /// strictNullChecks mode if they need a type for the sent value.
    /// NOTE: In non-strictNullChecks mode, `undefined` (the default sent value)
    /// is assignable to everything.
    cannot_fallback_to_iterable_iterator: bool,

    allow_new_target: bool,

    disallow_suggesting_property_on_no_var: bool,

    /// Should be modified directly instead of using `with_ctx`.
    in_unreachable: bool,

    /// `true` for top-level type annotations.
    is_not_topmost_type: bool,

    is_fn_param: bool,

    in_module: bool,

    checking_switch_discriminant_as_bin: bool,

    /// If true, obj of the expression statement is `super` keyword.
    obj_is_super: bool,

    use_properties_of_this_implicitly: bool,

    is_type_ann_for_call_reeval_chosen_from_overload: bool,

    is_type_predicate: bool,

    /// True if validating object properties that have get accessors
    get_accessor_prop: bool,

    /// True if validating object properties that have set accessors
    set_accessor_prop: bool,
}

impl Ctx {
    pub fn reevaluating(self) -> bool {
        self.reevaluating_argument || self.reevaluating_call_or_new
    }

    pub fn can_generalize_literals(self) -> bool {
        !self.in_const_assertion && !self.in_argument && !self.in_cond
    }

    fn is_static(&self) -> bool {
        self.in_static_block || self.in_static_method || self.in_static_property_initializer
    }
}

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
pub struct Analyzer<'scope, 'b> {
    env: Env,
    pub(crate) cm: Arc<SourceMap>,

    comments: StcComments,

    /// This is [None] only for `.d.ts` files.
    pub mutations: Option<Mutations>,

    storage: Storage<'b>,

    export_equals_span: Span,

    scope: Scope<'scope>,

    ctx: Ctx,

    loader: &'b dyn Load,

    pub(crate) config: InnerConfig,

    cur_facts: Facts,

    /// Used while inferring types.
    mapped_type_param_name: Vec<Id>,

    debugger: Option<Debugger>,

    data: Box<AnalyzerData>,

    destructure_count: Rc<Cell<DestructureId>>,
}

/// This type **should be boxed** for performance.
#[derive(Debug, Default)]
struct AnalyzerData {
    imports_by_id: FxHashMap<Id, ModuleInfo>,

    /// Value should [Type::Arc] of [Type::Module]
    imports: FxHashMap<(ModuleId, ModuleId), Type>,
    /// See docs of ModuleItemMut for documentation.
    prepend_stmts: Vec<RStmt>,

    /// See docs of ModuleItemMut for documentation.
    append_stmts: Vec<RStmt>,

    unmergable_type_decls: FxHashMap<Id, Vec<(Span, usize)>>,

    /// Used to check mixed exports.
    ///
    /// e.g. `A` for `type A = {}`
    local_type_decls: FxHashMap<Id, Vec<Span>>,

    /// Used to check mixed exports.
    ///
    /// e.g. `A` for `export type A = {}`
    exported_type_decls: FxHashMap<Id, Vec<Span>>,

    /// Filled only once, by `fill_known_type_names`.
    bindings: Bindings,

    unresolved_imports: FxHashSet<Id>,

    /// Spans of declared variables.
    var_spans: FxHashMap<Id, Vec<(VarKind, Span)>>,

    /// Spans of functions **with body**.
    fn_impl_spans: FxHashMap<Id, Vec<Span>>,

    /// One instance of each module (typescript `module` keyword).
    for_module: PerModuleData,

    /// When multiple overloads are wrong, tsc reports an error only for first
    /// one.
    ///
    /// We mimic it by storing names of wrong overloads.
    /// Only first wrong overload should be added to this set.
    known_wrong_overloads: FxHashSet<Id>,

    cache: TypeCache,

    checked_for_async_iterator: bool,

    /// Used to check mixed default exports.
    merged_default_exports: FxHashSet<Id>,

    jsx_prop_name: Option<Option<JsWord>>,
}

/// Configuration for the analyzer.
#[derive(Debug, Default)]
pub(crate) struct InnerConfig {
    pub is_builtin: bool,

    pub is_dts: bool,
}

#[derive(Debug, Default)]
struct PerModuleData {
    /// Spans exported items.
    exports_spans: FxHashMap<(JsWord, IdCtx), Vec<Span>>,
}

// TODO(kdy1):
//#[validator] #[validator]impl Analyzer<'_, '_> {
//    type Output = ValidationResult<ty::Module>;
//
//    fn validate(&mut self, node: &mut Program) -> Self::Output {
//        match node {
//            Program::Module(m) => m.validate_with(self),
//            Program::Script(s) => s.validate_with(self),
//        }
//    }
//}

fn make_module_ty(span: Span, name: RTsModuleName, exports: ModuleTypeData) -> ty::Module {
    ty::Module {
        span,
        name,
        exports: box exports,
        metadata: Default::default(),
        tracker: Default::default(),
    }
}

// TODO(kdy1):
//#[validator] #[validator]impl Analyzer<'_, '_> {
//    type Output = ValidationResult<ty::Module>;
//
//    fn validate(&mut self, node: &mut Module) -> Self::Output {
//        let span = node.span;
//
//        let mut new = self.new(Scope::root());
//        node.visit_mut_children_with(&mut new);
//        self.info.errors.append_errors(&mut new.info.errors);
//        println!("after visit children");
//
//        Ok(self.finalize(make_module_ty(span, new.info.exports)))
//    }
//}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RScript) -> VResult<ty::Module> {
        let span = node.span;

        let (errors, data) = {
            let mut new = self.new(Scope::root(), Default::default());
            {
                node.visit_children_with(&mut new);
            }

            let errors = new.storage.take_errors();
            let data = new.storage.take_info(self.ctx.module_id);

            (errors, data)
        };
        self.storage.report_all(errors);

        Ok(make_module_ty(
            span,
            RTsModuleName::Str(RStr {
                span: DUMMY_SP,
                value: js_word!(""),
                raw: None,
            }),
            data,
        ))
    }
}

fn _assert_types() {
    fn is_sync<T: Sync>() {}
    fn is_send<T: Send>() {}
    is_sync::<Info>();
    is_send::<Info>();
}

impl<'scope, 'b> Analyzer<'scope, 'b> {
    pub fn root(
        env: Env,
        cm: Arc<SourceMap>,
        comments: StcComments,
        mut storage: Storage<'b>,
        loader: &'b dyn Load,
        debugger: Option<Debugger>,
    ) -> Self {
        if env.rule().use_define_property_for_class_fields && env.target() == EsVersion::Es3 {
            storage.report(ErrorKind::OptionInvalidForEs3 { span: DUMMY_SP }.into())
        }

        Self::new_inner(
            env,
            cm,
            comments,
            storage,
            Some(Default::default()),
            loader,
            Scope::root(),
            false,
            debugger,
            Default::default(),
        )
    }

    pub(crate) fn for_builtin(env: StableEnv, storage: &'b mut Builtin) -> Self {
        Self::new_inner(
            Env::new(env, Default::default(), EsVersion::latest(), ModuleConfig::None, Default::default()),
            Arc::new(SourceMap::default()),
            Default::default(),
            box storage,
            None,
            &NoopLoader,
            Scope::root(),
            true,
            None,
            Default::default(),
        )
    }

    #[allow(clippy::wrong_self_convention)]
    fn new(&'b self, scope: Scope<'scope>, data: Box<AnalyzerData>) -> Self {
        Self::new_inner(
            self.env.clone(),
            self.cm.clone(),
            self.comments.clone(),
            self.storage.subscope(),
            None,
            self.loader,
            scope,
            self.config.is_builtin,
            self.debugger.clone(),
            data,
        )
    }

    fn new_inner(
        env: Env,
        cm: Arc<SourceMap>,
        comments: StcComments,
        storage: Storage<'b>,
        mutations: Option<Mutations>,
        loader: &'b dyn Load,
        scope: Scope<'scope>,
        is_builtin: bool,
        debugger: Option<Debugger>,
        data: Box<AnalyzerData>,
    ) -> Self {
        let is_dts = storage.is_dts();

        Self {
            env,
            cm,
            comments,
            storage,
            mutations,
            export_equals_span: DUMMY_SP,
            scope,
            config: InnerConfig { is_builtin, is_dts },
            ctx: Ctx {
                module_id: ModuleId::builtin(),
                in_class_member: false,
                in_const_assertion: false,
                in_constructor_param: false,
                disallow_unknown_object_property: false,
                use_undefined_for_empty_array_lit: false,
                allow_module_var: false,
                check_for_implicit_any: false,
                array_lit_cannot_be_tuple: false,
                prefer_tuple_for_array_lit: false,
                in_shorthand: false,
                is_instantiating_class: false,
                in_cond: false,
                should_store_truthy_for_access: false,
                in_switch_case_test: false,
                in_computed_prop_name: false,
                in_opt_chain: false,
                in_declare: is_dts,
                in_fn_without_body: false,
                in_global: !is_builtin && is_dts,
                in_export_assignment: false,
                in_export_default_expr: false,
                in_async: false,
                in_generator: false,
                is_calling_iife: false,
                in_useless_expr_for_seq: false,
                in_ts_fn_type: false,
                in_actual_type: false,
                report_error_for_non_local_vars: false,
                in_static_property_initializer: false,
                in_static_block: false,
                in_static_method: false,
                reevaluating_call_or_new: false,
                reevaluating_argument: false,
                ignore_errors: false,
                var_kind: VarDeclKind::Var,
                pat_mode: PatMode::Assign,
                computed_prop_mode: ComputedPropMode::Object,
                allow_ref_declaring: false,
                in_argument: false,
                in_fn_with_return_type: false,
                in_return_arg: false,
                in_assign_rhs: false,
                in_export_decl: false,
                in_export_named: false,
                skip_identical_while_inference: false,
                super_references_super_class: false,
                in_class_with_super: false,
                cannot_fallback_to_iterable_iterator: false,
                allow_new_target: false,
                disallow_suggesting_property_on_no_var: false,
                in_unreachable: false,
                is_not_topmost_type: false,
                is_fn_param: false,
                in_module: false,
                checking_switch_discriminant_as_bin: false,
                obj_is_super: false,
                use_properties_of_this_implicitly: false,
                is_type_ann_for_call_reeval_chosen_from_overload: false,
                is_type_predicate: false,
                get_accessor_prop: false,
                set_accessor_prop: false,
            },
            loader,
            cur_facts: Default::default(),
            mapped_type_param_name: vec![],
            debugger,
            data,
            destructure_count: Default::default(),
        }
    }

    #[inline]
    pub(crate) fn with<F, Ret>(&mut self, op: F) -> Ret
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> Ret,
    {
        op(self)
    }

    pub(crate) fn with_scope_for_type_params<F, Ret>(&mut self, op: F) -> Ret
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> Ret,
    {
        self.with_child(ScopeKind::TypeParams, Default::default(), |a: &mut Analyzer| {
            // TODO(kdy1): Optimize this.
            Ok(op(a))
        })
        .unwrap()
    }

    pub(crate) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> VResult<Ret>
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> VResult<Ret>,
    {
        self.with_child_with_hook(kind, facts, op, |_| {})
    }

    ///
    ///
    ///
    /// Hook is invoked with `self` (not child) after `op`.
    pub(crate) fn with_child_with_hook<F, Ret, H>(&mut self, kind: ScopeKind, facts: CondFacts, op: F, hook: H) -> VResult<Ret>
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> VResult<Ret>,
        H: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>),
    {
        let ctx = self.ctx;
        let mutations = self.mutations.take();
        let cur_facts = take(&mut self.cur_facts);
        let module_data = if kind == ScopeKind::Module {
            take(&mut self.data.for_module)
        } else {
            Default::default()
        };
        let data = take(&mut self.data);

        let child_scope = Scope::new(&self.scope, kind, facts);
        let (ret, errors, cur_facts, mut child_scope, mutations, data) = {
            let mut child = self.new(child_scope, data);
            child.mutations = mutations;
            child.cur_facts = cur_facts;
            child.ctx = ctx;

            let ret = op(&mut child);

            let errors = if child.ctx.ignore_errors {
                Default::default()
            } else {
                child.storage.take_errors()
            };

            (
                ret,
                errors,
                child.cur_facts,
                child.scope.remove_parent(),
                child.mutations.take(),
                take(&mut child.data),
            )
        };
        self.storage.report_all(errors);

        self.cur_facts = cur_facts;
        self.mutations = mutations;
        self.data = data;

        hook(self);

        self.scope.move_types_from_child(&mut child_scope);
        self.scope.move_vars_from_child(&mut child_scope);
        if kind == ScopeKind::Module {
            self.data.for_module = module_data;
        }

        // Move return types from child to parent
        match kind {
            // These kinds of scope eats return statements
            ScopeKind::Module | ScopeKind::Method { .. } | ScopeKind::ArrowFn | ScopeKind::Fn => {}
            _ => {
                self.scope.return_values += child_scope.return_values;
            }
        }

        ret
    }

    /// Used for debugging. Returns the line and column of `span.lo` in form of
    /// `(line, column)`.
    fn line_col(&self, span: Span) -> String {
        if span.is_dummy() {
            return "".into();
        }
        let lo = self.cm.lookup_char_pos(span.lo);
        let hi = self.cm.lookup_char_pos(span.hi);
        format!("({}:{}-{}:{})", lo.line, lo.col_display + 1, hi.line, hi.col_display + 1)
    }

    fn validate_with<F>(&mut self, op: F)
    where
        F: FnOnce(&mut Analyzer) -> VResult<()>,
    {
        let res = op(self);
        match res {
            Ok(()) => {}
            Err(err) => {
                self.storage.report(err);
            }
        }
    }

    fn with_ctx(&mut self, ctx: Ctx) -> WithCtx<'_, 'scope, 'b> {
        let orig_ctx = self.ctx;
        self.ctx = ctx;
        WithCtx { analyzer: self, orig_ctx }
    }

    fn rule(&self) -> Rule {
        self.env.rule()
    }

    fn marks(&self) -> Marks {
        self.env.shared().marks()
    }
}

pub(super) struct WithCtx<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
    orig_ctx: Ctx,
}

impl Drop for WithCtx<'_, '_, '_> {
    fn drop(&mut self) {
        self.analyzer.ctx = self.orig_ctx;
    }
}

impl<'b, 'c> Deref for WithCtx<'_, 'b, 'c> {
    type Target = Analyzer<'b, 'c>;

    fn deref(&self) -> &Self::Target {
        self.analyzer
    }
}

impl<'b, 'c> DerefMut for WithCtx<'_, 'b, 'c> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.analyzer
    }
}

/// Panics if there's a load request.
pub struct NoopLoader;

impl Load for NoopLoader {
    fn module_id(&self, base: &Arc<FileName>, src: &str) -> Option<ModuleId> {
        unreachable!()
    }

    fn is_in_same_circular_group(&self, base: &Arc<FileName>, dep: &str) -> bool {
        unreachable!()
    }

    fn load_circular_dep(&self, base: &Arc<FileName>, dep: &str, partial: &ModuleTypeData) -> VResult<Type> {
        unreachable!()
    }

    fn load_non_circular_dep(&self, base: &Arc<FileName>, dep: &str) -> VResult<Type> {
        unreachable!()
    }

    fn declare_module(&self, name: &JsWord, module: Type) -> ModuleId {
        unreachable!()
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, modules: &Vec<RModule>) {
        self.ctx.in_module = true;

        let mut items = vec![];
        for m in modules {
            items.extend(&m.body);
        }
        // TODO: Pass spans.
        self.load_normal_imports(vec![], &items);

        self.fill_known_type_names(&modules);

        self.validate_stmts_with_hoisting(&items);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, m: &RModule) {
        self.ctx.in_module = true;
        let is_dts = self.config.is_dts;

        debug_assert!(GLOBALS.is_set(), "Analyzer requires swc_common::GLOBALS");

        let ctxt = self.storage.module_id(0);
        let path = self.storage.path(ctxt);

        let items_ref = m.body.iter().collect::<Vec<_>>();
        self.load_normal_imports(vec![(ctxt, m.span)], &items_ref);

        self.fill_known_type_names(&m.body);

        let mut has_normal_export = false;
        m.body.iter().for_each(|item| match item {
            RModuleItem::ModuleDecl(RModuleDecl::TsExportAssignment(decl)) => {
                if self.export_equals_span.is_dummy() {
                    self.export_equals_span = decl.span;
                }
                if !is_dts && has_normal_export {
                    self.storage
                        .report(ErrorKind::ExportEqualsMixedWithOtherExports { span: decl.span }.into());
                }

                //
            }
            RModuleItem::ModuleDecl(item) => match item {
                RModuleDecl::ExportDecl(..)
                | RModuleDecl::ExportAll(..)
                | RModuleDecl::ExportDefaultDecl(..)
                | RModuleDecl::ExportDefaultExpr(..)
                | RModuleDecl::TsNamespaceExport(..) => {
                    has_normal_export = true;
                    if !is_dts && !self.export_equals_span.is_dummy() {
                        self.storage.report(
                            ErrorKind::ExportEqualsMixedWithOtherExports {
                                span: self.export_equals_span,
                            }
                            .into(),
                        );
                    }
                }
                _ => {}
            },
            _ => {}
        });

        if !self.ctx.in_declare {
            self.report_error_for_wrong_top_level_ambient_fns(&m.body);
        }

        if self.config.is_builtin {
            m.body.visit_children_with(self);
        } else {
            self.validate_stmts_and_collect(&items_ref);
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, items: &Vec<RStmt>) {
        self.fill_known_type_names(&items);

        for item in items.iter() {
            item.visit_with(self);
        }

        Ok(())
    }
}

/// Done
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &RDecorator) {
        d.expr.validate_with_default(self).report(&mut self.storage);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &RTsImportEqualsDecl) {
        let ctxt = self.ctx.module_id;

        let ctx = Ctx {
            in_declare: self.ctx.in_declare || node.declare,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|analyzer: &mut Analyzer| {
            let (var_ty, type_ty) = match node.module_ref {
                RTsModuleRef::TsEntityName(ref e) => {
                    let var_ty = analyzer.resolve_typeof(node.span, e).map(|ty| ty.freezed());

                    let type_ty = analyzer
                        .type_of_ts_entity_name(node.span, &e.clone().into(), None)
                        .convert_err(|err| match err {
                            ErrorKind::TypeNotFound {
                                span,
                                name,
                                ctxt,
                                type_args,
                            } => ErrorKind::NamespaceNotFound {
                                span,
                                name,
                                ctxt,
                                type_args,
                            },
                            _ => err,
                        })
                        .map(|ty| ty.freezed());

                    (var_ty, type_ty)
                }
                RTsModuleRef::TsExternalModuleRef(ref e) => {
                    let (dep, data) = analyzer.get_imported_items(e.span, &e.expr.value);
                    data.assert_clone_cheap();

                    // Import successful
                    if ctxt != dep {
                        let module_ty = analyzer
                            .data
                            .imports
                            .get(&(ctxt, dep))
                            .cloned()
                            .unwrap_or_else(|| Type::any(e.span, Default::default()));

                        let module_ty = analyzer
                            .access_property(
                                node.span,
                                &module_ty,
                                &Key::Normal {
                                    span: node.span,
                                    sym: js_word!("default"),
                                },
                                expr::TypeOfMode::RValue,
                                IdCtx::Type,
                                Default::default(),
                            )
                            .unwrap_or(module_ty)
                            .freezed();

                        (Ok(module_ty.clone()), Ok(module_ty))
                    } else {
                        (Ok(Type::any(e.span, Default::default())), Ok(Type::any(e.span, Default::default())))
                    }
                }
            };

            #[allow(clippy::unnecessary_unwrap)]
            if var_ty.is_err() && type_ty.is_err() {
                analyzer.storage.report(type_ty.unwrap_err());
                return Ok(());
            }

            if let Ok(ty) = type_ty {
                ty.assert_clone_cheap();

                analyzer.register_type(node.id.clone().into(), ty.clone());
                if node.is_export {
                    analyzer
                        .storage
                        .export_type(node.span, analyzer.ctx.module_id, node.id.sym.clone(), ty)
                }
            }

            if let Ok(ty) = var_ty {
                ty.assert_clone_cheap();

                analyzer.declare_var(
                    node.span,
                    VarKind::Import,
                    node.id.clone().into(),
                    Some(ty.clone()),
                    None,
                    true,
                    false,
                    false,
                    false,
                )?;

                analyzer
                    .storage
                    .export_var(node.span, analyzer.ctx.module_id, node.id.sym.clone(), ty)
            }

            Ok(())
        })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsModuleBlock) -> VResult<()> {
        let body = decl.body.iter().collect();
        self.validate_stmts_with_hoisting(&body);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsNamespaceDecl) -> VResult<Type> {
        let is_builtin = self.config.is_builtin;
        let span = decl.span;
        let ctxt = self.ctx.module_id;

        let ctx = Ctx {
            in_global: self.ctx.in_global || decl.global,
            in_declare: self.ctx.in_declare || decl.declare,
            ..self.ctx
        };

        self.with_ctx(ctx)
            .with_child(ScopeKind::Module, Default::default(), |a: &mut Analyzer| {
                //

                decl.body.visit_with(a);

                let exports = a.storage.take_info(ctxt);

                let ty = Namespace {
                    name: decl.id.clone().into(),
                    span,
                    exports: box exports,
                    metadata: Default::default(),
                    tracker: Default::default(),
                };
                let ty = Type::Namespace(ty).freezed();

                Ok(ty)
            })
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsModuleDecl) -> VResult<Option<Type>> {
        let is_builtin = self.config.is_builtin;
        let span = decl.span;
        let ctxt = self.ctx.module_id;
        let global = decl.global;

        let ctx = Ctx {
            in_global: global,
            in_declare: self.ctx.in_declare || decl.declare,
            ..self.ctx
        };
        let mut ty = self
            .with_ctx(ctx)
            .with_child(ScopeKind::Module, Default::default(), |child: &mut Analyzer| {
                child.scope.cur_module_name = match &decl.id {
                    RTsModuleName::Ident(i) => Some(i.into()),
                    RTsModuleName::Str(_) => None,
                };

                if decl.body.is_none() {
                    return Ok(Some(Type::any(span, Default::default())));
                }

                decl.visit_children_with(child);

                let mut exports = child.storage.take_info(ctxt);
                // Ambient module members are always exported with or without export keyword
                if is_builtin || decl.declare {
                    for (id, var) in take(&mut exports.private_vars) {
                        var.assert_valid();

                        if !exports.vars.contains_key(id.sym()) {
                            exports.vars.insert(id.sym().clone(), var);
                        }
                    }

                    for (id, ty) in take(&mut exports.private_types) {
                        for ty in &ty {
                            ty.assert_valid();
                        }

                        if !exports.types.contains_key(id.sym()) {
                            exports.types.insert(id.sym().clone(), ty);
                        }
                    }
                }

                if is_builtin || !global {
                    let ty = ty::Module {
                        name: decl.id.clone(),
                        span,
                        exports: box exports,
                        metadata: Default::default(),
                        tracker: Default::default(),
                    };
                    let ty = Type::Module(ty).freezed();
                    return Ok(Some(ty));
                }

                Ok(None)
            })?;

        ty.freeze();

        if let Some(ty) = &ty {
            match &decl.id {
                RTsModuleName::Ident(i) => {
                    self.register_type(i.into(), ty.clone());
                }
                RTsModuleName::Str(s) => {
                    let name: &str = &s.value;

                    if let Some(pos) = name.as_bytes().iter().position(|&c| c == b'*') {
                        if let Some(rpos) = name.as_bytes().iter().rposition(|&c| c == b'*') {
                            if pos != rpos {
                                self.storage.report(ErrorKind::TooManyAsterisk { span: s.span }.into());
                            }
                        }
                    }

                    let module_id = self.loader.declare_module(&s.value, ty.clone());

                    self.insert_import_info(ctxt, module_id, ty.clone()).report(&mut self.storage);
                }
            }
        }

        Ok(ty)
    }
}
