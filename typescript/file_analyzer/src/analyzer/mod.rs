pub use self::marks::Marks;
pub(crate) use self::scope::ScopeKind;
use self::{
    control_flow::{CondFacts, Facts},
    pat::PatMode,
    props::ComputedPropMode,
    scope::Scope,
    util::ResultExt,
};
use crate::{
    env::{Env, StableEnv},
    loader::{Load, ModuleInfo},
    ty,
    ty::Type,
    validator,
    validator::ValidateWith,
    DepInfo, Rule, ValidationResult,
};
use fxhash::FxHashMap;
use rnode::VisitWith;
use slog::Logger;
use stc_ts_ast_rnode::RDecorator;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RModule;
use stc_ts_ast_rnode::RModuleDecl;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RScript;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RStr;
use stc_ts_ast_rnode::RTsImportEqualsDecl;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_ast_rnode::RTsModuleName;
use stc_ts_ast_rnode::RTsModuleRef;
use stc_ts_ast_rnode::RTsNamespaceDecl;
use stc_ts_dts_mutations::Mutations;
use stc_ts_errors::debug::debugger::Debugger;
use stc_ts_errors::debug::duplicate::DuplicateTracker;
use stc_ts_errors::Error;
use stc_ts_storage::Builtin;
use stc_ts_storage::Info;
use stc_ts_storage::Storage;
use stc_ts_types::{Id, ModuleId, ModuleTypeData, SymbolIdGenerator};
use std::mem::take;
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::Arc,
};
use swc_atoms::js_word;
use swc_atoms::JsWord;
use swc_common::{SourceMap, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_parser::JscTarget;

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
mod enums;
mod export;
mod expr;
mod finalizer;
mod function;
mod generalize;
mod generic;
mod hoisting;
mod import;
mod marks;
mod pat;
mod props;
mod scope;
mod stmt;
#[cfg(test)]
mod tests;
mod types;
mod util;
mod visit_mut;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ctx {
    module_id: ModuleId,

    allow_module_var: bool,

    /// `true` for condition of conditional expression or of an if statement.
    in_cond: bool,
    in_switch_case_test: bool,

    in_declare: bool,
    in_fn_without_body: bool,
    in_global: bool,
    in_export_default_expr: bool,

    var_kind: VarDeclKind,
    pat_mode: PatMode,
    computed_prop_mode: ComputedPropMode,
    allow_ref_declaring: bool,
    in_argument: bool,
    preserve_ref: bool,

    /// Used before calling `access_property`, which does not accept `Ref` as an
    /// input.
    ///
    ///
    /// Note: Reference type in top level intersections are treated as
    /// top-level types.
    ignore_expand_prevention_for_top: bool,

    ignore_expand_prevention_for_all: bool,

    /// If true, `expand` and `expand_fully` will not expand function
    /// parameters.
    preserve_params: bool,

    /// If true, `expand` and `expand_fully` will not expand function
    /// parameters.
    preserve_ret_ty: bool,

    /// If true, **recovereable** errors are ignored. Used for trying.
    ignore_errors: bool,

    /// If true, assignemt from `{ a: string }` to `{}` will fail.
    fail_on_extra_fields: bool,

    skip_union_while_inferencing: bool,
}

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
pub struct Analyzer<'scope, 'b> {
    logger: Logger,
    env: Env,
    cm: Arc<SourceMap>,

    /// This is [None] only for `.d.ts` files.
    pub mutations: Option<Mutations>,

    storage: Storage<'b>,

    export_equals_span: Span,

    imports_by_id: FxHashMap<Id, ModuleInfo>,

    pending_exports: Vec<((Id, Span), RExpr)>,

    imports: FxHashMap<(ModuleId, ModuleId), Arc<ModuleTypeData>>,
    /// See docs of ModuleitemMut for documentation.
    prepend_stmts: Vec<RStmt>,

    /// See docs of ModuleitemMut for documentation.
    append_stmts: Vec<RStmt>,

    scope: Scope<'scope>,

    ctx: Ctx,

    loader: &'b dyn Load,

    is_builtin: bool,

    duplicated_tracker: DuplicateTracker,

    cur_facts: Facts,

    symbols: Arc<SymbolIdGenerator>,

    /// Used while inferencing types.
    mapped_type_param_name: Vec<Id>,

    debugger: Option<Debugger>,
}

/// TODO
const NO_DUP: bool = false;

impl Analyzer<'_, '_> {
    /// Mark node as visited. This method panics if Analyzer had visited node.
    fn record<N>(&mut self, node: &N)
    where
        N: Debug + Spanned,
    {
        if cfg!(debug_assertions) && NO_DUP {
            self.duplicated_tracker.record(node)
        }
    }
}

// TODO:
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
    }
}

// TODO:
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
    fn validate(&mut self, node: &RScript) -> ValidationResult<ty::Module> {
        let span = node.span;

        let (errors, data) = {
            let mut new = self.new(Scope::root(self.logger.clone()));
            {
                node.visit_children_with(&mut new);
            }

            let errors = new.storage.take_errors();
            let data = new.storage.take_info(self.ctx.module_id);

            (errors, data)
        };
        self.storage.report_all(errors);

        Ok(self.finalize(make_module_ty(
            span,
            RTsModuleName::Str(RStr {
                span: DUMMY_SP,
                has_escape: false,
                kind: Default::default(),
                value: js_word!(""),
            }),
            data,
        )))
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
        logger: Logger,
        env: Env,
        cm: Arc<SourceMap>,
        storage: Storage<'b>,
        loader: &'b dyn Load,
        debugger: Option<Debugger>,
    ) -> Self {
        Self::new_inner(
            logger.clone(),
            env,
            cm,
            storage,
            Some(Default::default()),
            loader,
            Scope::root(logger),
            false,
            Default::default(),
            debugger,
        )
    }

    pub(crate) fn for_builtin(env: StableEnv, storage: &'b mut Builtin) -> Self {
        let logger = env.logger_for_builtin();

        Self::new_inner(
            logger.clone(),
            Env::new(env, Default::default(), JscTarget::Es2020, Default::default()),
            Arc::new(SourceMap::default()),
            box storage,
            None,
            &NoopLoader,
            Scope::root(logger),
            true,
            Default::default(),
            None,
        )
    }

    fn new(&'b self, scope: Scope<'scope>) -> Self {
        Self::new_inner(
            self.logger.clone(),
            self.env.clone(),
            self.cm.clone(),
            self.storage.subscope(),
            None,
            self.loader,
            scope,
            self.is_builtin,
            self.symbols.clone(),
            self.debugger.clone(),
        )
    }

    fn new_inner(
        logger: Logger,
        env: Env,
        cm: Arc<SourceMap>,
        storage: Storage<'b>,
        mutations: Option<Mutations>,
        loader: &'b dyn Load,
        scope: Scope<'scope>,
        is_builtin: bool,
        symbols: Arc<SymbolIdGenerator>,
        debugger: Option<Debugger>,
    ) -> Self {
        Self {
            logger,
            env,
            cm,
            storage,
            mutations,
            export_equals_span: DUMMY_SP,
            imports: Default::default(),
            pending_exports: Default::default(),
            prepend_stmts: Default::default(),
            append_stmts: Default::default(),
            scope,
            ctx: Ctx {
                module_id: ModuleId::builtin(),
                allow_module_var: false,
                in_cond: false,
                in_switch_case_test: false,
                in_declare: false,
                in_fn_without_body: false,
                in_global: false,
                in_export_default_expr: false,
                var_kind: VarDeclKind::Var,
                pat_mode: PatMode::Assign,
                computed_prop_mode: ComputedPropMode::Object,
                allow_ref_declaring: false,
                in_argument: false,
                preserve_ref: false,
                ignore_expand_prevention_for_top: false,
                ignore_expand_prevention_for_all: false,
                preserve_params: false,
                preserve_ret_ty: false,
                ignore_errors: false,
                fail_on_extra_fields: false,
                skip_union_while_inferencing: false,
            },
            loader,
            is_builtin,
            duplicated_tracker: Default::default(),
            cur_facts: Default::default(),
            symbols,
            mapped_type_param_name: vec![],
            imports_by_id: Default::default(),
            debugger,
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
            // TODO: Optimize this.
            Ok(op(a))
        })
        .unwrap()
    }

    /// TODO: Move return values to parent scope
    pub(crate) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> ValidationResult<Ret>
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> ValidationResult<Ret>,
    {
        let ctx = self.ctx;
        let imports = take(&mut self.imports);
        let imports_by_id = take(&mut self.imports_by_id);
        let mutations = self.mutations.take();
        let cur_facts = take(&mut self.cur_facts);

        let child_scope = Scope::new(&self.scope, kind, facts);
        let (
            ret,
            errors,
            imports,
            imports_by_id,
            cur_facts,
            mut child_scope,
            dup,
            prepend_stmts,
            append_stmts,
            mutations,
        ) = {
            let mut child = self.new(child_scope);
            child.imports = imports;
            child.imports_by_id = imports_by_id;
            child.mutations = mutations;
            child.cur_facts = cur_facts;
            child.ctx = ctx;

            let ret = op(&mut child);

            (
                ret,
                child.storage.take_errors(),
                child.imports,
                child.imports_by_id,
                child.cur_facts,
                child.scope.remove_parent(),
                child.duplicated_tracker,
                child.prepend_stmts,
                child.append_stmts,
                child.mutations.take(),
            )
        };
        self.storage.report_all(errors);

        self.imports = imports;
        self.imports_by_id = imports_by_id;
        self.cur_facts = cur_facts;
        self.mutations = mutations;

        // if !self.is_builtin {
        //     assert_eq!(
        //         info.exports.types,
        //         Default::default(),
        //         "child cannot export a type"
        //     );
        //     assert!(
        //         info.exports.vars.is_empty(),
        //         "child cannot export a variable"
        //     );
        // }

        self.duplicated_tracker.record_all(dup);
        self.scope.copy_hoisted_vars_from(&mut child_scope);
        self.prepend_stmts.extend(prepend_stmts);
        self.append_stmts.extend(append_stmts);

        // Move return types from child to parent
        match kind {
            // These kinds of scope eats return statements
            ScopeKind::Module | ScopeKind::Method | ScopeKind::ArrowFn | ScopeKind::Fn => {}
            _ => {
                self.scope.return_values += child_scope.return_values;
            }
        }

        ret
    }

    fn with_ctx(&mut self, ctx: Ctx) -> WithCtx<'_, 'scope, 'b> {
        let orig_ctx = self.ctx;
        self.ctx = ctx;
        WithCtx {
            analyzer: self,
            orig_ctx,
        }
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
        &self.analyzer
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
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool {
        false
    }

    fn load_non_circular_dep(&self, base: Arc<PathBuf>, import: &DepInfo) -> ValidationResult<ModuleInfo> {
        unimplemented!()
    }

    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> ValidationResult<ModuleInfo> {
        unimplemented!()
    }

    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId {
        unimplemented!()
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, modules: &Vec<RModule>) {
        let mut items = vec![];
        for m in modules {
            items.extend(&m.body);
        }
        self.load_normal_imports(&items);

        self.validate_stmts_with_hoisting(&items);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, items: &Vec<RModuleItem>) {
        let mut items_ref = items.iter().collect::<Vec<_>>();
        self.load_normal_imports(&items_ref);

        let mut has_normal_export = false;
        items.iter().for_each(|item| match item {
            RModuleItem::ModuleDecl(RModuleDecl::TsExportAssignment(decl)) => {
                if self.export_equals_span.is_dummy() {
                    self.export_equals_span = decl.span;
                }
                if has_normal_export {
                    self.storage.report(box Error::TS2309 { span: decl.span });
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
                    if !self.export_equals_span.is_dummy() {
                        self.storage.report(box Error::TS2309 {
                            span: self.export_equals_span,
                        });
                    }
                }
                _ => {}
            },
            _ => {}
        });

        if !self.ctx.in_declare {
            self.validate_ambient_fns(&items);
        }

        if self.is_builtin {
            items.visit_children_with(self);
        } else {
            self.validate_stmts_and_collect(&items_ref);
        }

        self.handle_pending_exports();

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, items: &Vec<RStmt>) {
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
        self.record(node);

        match node.module_ref {
            RTsModuleRef::TsEntityName(ref e) => {
                match self.type_of_ts_entity_name(node.span, self.ctx.module_id, e, None) {
                    Ok(..) => {}
                    Err(err) => self.storage.report(err),
                }
            }
            _ => {}
        }

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsNamespaceDecl) {
        todo!("namespace is not supported yet")
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &RTsModuleDecl) {
        let span = decl.span;
        let ctxt = self.ctx.module_id;
        let global = decl.global;

        let ctx = Ctx {
            in_global: global,
            in_declare: self.ctx.in_declare || decl.declare,
            ..self.ctx
        };
        let ty = self
            .with_ctx(ctx)
            .with_child(ScopeKind::Module, Default::default(), |child: &mut Analyzer| {
                child.scope.cur_module_name = match &decl.id {
                    RTsModuleName::Ident(i) => Some(i.into()),
                    RTsModuleName::Str(_) => None,
                };

                decl.visit_children_with(child);

                let mut exports = child.storage.take_info(ctxt);
                // Ambient module members are always exported with or without export keyword
                if decl.declare {
                    for (id, var) in take(&mut exports.private_vars) {
                        if !exports.vars.contains_key(id.sym()) {
                            exports.vars.insert(id.sym().clone(), var);
                        }
                    }

                    for (id, ty) in take(&mut exports.private_types) {
                        if !exports.types.contains_key(id.sym()) {
                            exports.types.insert(id.sym().clone(), ty);
                        }
                    }
                }

                if !global {
                    let ty = child.finalize(ty::Module {
                        name: decl.id.clone(),
                        span,
                        exports: box exports,
                    });
                    let ty = Type::Module(ty).cheap();
                    return Ok(Some(ty));
                }

                Ok(None)
            })?;

        if let Some(ty) = ty {
            match &decl.id {
                RTsModuleName::Ident(i) => {
                    self.register_type(i.into(), ty);
                }
                RTsModuleName::Str(s) => {
                    //TODO
                    return Ok(());
                }
            }
        }

        Ok(())
    }
}
