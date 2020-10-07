pub use self::marks::Marks;
pub(crate) use self::scope::ScopeKind;
use self::{
    control_flow::{CondFacts, Facts},
    import::ImportFinder,
    pat::PatMode,
    props::ComputedPropMode,
    scope::Scope,
    stmt::AmbientFunctionHandler,
    util::ResultExt,
};
use crate::env::BuiltIn;
use crate::mode::Builtin;
use crate::mode::Mode;
use crate::mode::Storage;
use crate::{
    debug::duplicate::DuplicateTracker,
    env::{Env, StableEnv},
    errors::{Error, Errors},
    loader::{Load, ModuleInfo},
    ty,
    ty::Type,
    validator,
    validator::{Validate, ValidateWith},
    DepInfo, Rule, Specifier, ValidationResult,
};
use bitflags::_core::mem::{replace, take};
use fxhash::{FxHashMap, FxHashSet};
use rayon::prelude::*;
use slog::Logger;
use stc_builtin_types::Lib;
use stc_types::{Id, ModuleId, ModuleTypeData, SymbolIdGenerator};
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::{Mark, SourceMap, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::{ModuleItem, *};
use swc_ecma_parser::JscTarget;
use swc_ecma_visit::{VisitMutWith, VisitWith};

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
mod util;
mod visit_mut;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ctx {
    module_id: ModuleId,

    in_declare: bool,
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
}

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
pub struct Analyzer<'a, 'b> {
    logger: Logger,
    env: Env,
    cm: Arc<SourceMap>,

    storage: Storage<'b>,

    export_equals_span: Span,
    in_declare: bool,

    imports_by_id: FxHashMap<Id, ModuleInfo>,

    pending_exports: Vec<((Id, Span), Expr)>,

    imports: FxHashMap<(ModuleId, ModuleId), Arc<ModuleTypeData>>,

    /// Used to handle
    ///
    /// ```ts
    /// declare function Mix<T, U>(c1: T, c2: U): T & U;
    /// class C1 extends Mix(Private, Private2) {
    /// }
    /// ```
    ///
    /// As code above becomes
    ///
    /// ```ts
    /// declare const C1_base: typeof Private & typeof Private2;
    /// declare class C1 extends C1_base {
    /// }
    /// ```
    ///
    /// we need to prepend statements.
    prepend_stmts: Vec<Stmt>,

    /// Used to handle
    ///
    /// ```ts
    /// export default function someFunc() {
    ///     return 'hello!';
    /// }
    ///
    /// someFunc.someProp = 'yo';
    /// ```
    ///
    /// As the code above becomes
    ///
    /// ```ts
    /// declare function someFunc(): string;
    /// declare namespace someFunc {
    ///     var someProp: string;
    /// }
    /// export default someFunc;
    /// ```
    ///
    /// we need to append statements.
    append_stmts: Vec<Stmt>,

    scope: Scope<'a>,

    ctx: Ctx,

    loader: &'b dyn Load,

    is_builtin: bool,

    duplicated_tracker: DuplicateTracker,

    cur_facts: Facts,

    symbols: Arc<SymbolIdGenerator>,

    /// Used while inferencing types.
    mapped_type_param_name: Vec<Id>,
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

#[derive(Debug, Default)]
pub struct Info {
    pub errors: Errors,
    pub exports: ty::ModuleTypeData,
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

fn make_module_ty(span: Span, exports: ModuleTypeData) -> ty::Module {
    ty::Module { span, exports }
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
    fn validate(&mut self, node: &mut Script) -> ValidationResult<ty::Module> {
        let span = node.span;

        let (errors, data) = {
            let mut new = self.new(Scope::root(self.logger.clone()));
            node.visit_mut_children_with(&mut new);
            let errors = new.storage.take_errors();
            let data = new.storage.take_info(self.ctx.module_id);

            (errors, data)
        };
        self.storage.report_all(errors);

        Ok(self.finalize(make_module_ty(span, data)))
    }
}

fn _assert_types() {
    fn is_sync<T: Sync>() {}
    fn is_send<T: Send>() {}
    is_sync::<Info>();
    is_send::<Info>();
}

impl<'a, 'b> Analyzer<'a, 'b> {
    pub fn root(
        logger: Logger,
        env: Env,
        cm: Arc<SourceMap>,
        storage: Storage<'b>,
        loader: &'b dyn Load,
    ) -> Self {
        Self::new_inner(
            logger.clone(),
            env,
            cm,
            storage,
            loader,
            Scope::root(logger),
            false,
            Default::default(),
        )
    }

    pub(crate) fn for_builtin(env: StableEnv, storage: &'b mut Builtin) -> Self {
        let logger = env.logger_for_builtin();

        Self::new_inner(
            logger.clone(),
            Env::new(
                env,
                Default::default(),
                JscTarget::Es2020,
                Default::default(),
            ),
            Arc::new(SourceMap::default()),
            box storage,
            &NoopLoader,
            Scope::root(logger),
            true,
            Default::default(),
        )
    }

    fn new(&'b self, scope: Scope<'a>) -> Self {
        Self::new_inner(
            self.logger.clone(),
            self.env.clone(),
            self.cm.clone(),
            self.storage.subscope(),
            self.loader,
            scope,
            self.is_builtin,
            self.symbols.clone(),
        )
    }

    fn new_inner(
        logger: Logger,
        env: Env,
        cm: Arc<SourceMap>,
        storage: Storage<'b>,
        loader: &'b dyn Load,
        scope: Scope<'a>,
        is_builtin: bool,
        symbols: Arc<SymbolIdGenerator>,
    ) -> Self {
        Self {
            logger,
            env,
            cm,
            storage,
            export_equals_span: DUMMY_SP,
            // builtin types are declared
            in_declare: is_builtin,
            imports: Default::default(),
            pending_exports: Default::default(),
            prepend_stmts: Default::default(),
            append_stmts: Default::default(),
            scope,
            ctx: Ctx {
                in_declare: false,
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
                module_id: ModuleId::builtin(),
            },
            loader,
            is_builtin,
            duplicated_tracker: Default::default(),
            cur_facts: Default::default(),
            symbols,
            mapped_type_param_name: vec![],
            imports_by_id: Default::default(),
        }
    }

    #[inline]
    pub(crate) fn with<F, Ret>(&mut self, op: F) -> Ret
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> Ret,
    {
        op(self)
    }

    /// TODO: Move return values to parent scope
    pub(crate) fn with_child<F, Ret>(
        &mut self,
        kind: ScopeKind,
        facts: CondFacts,
        op: F,
    ) -> ValidationResult<Ret>
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> ValidationResult<Ret>,
    {
        let ctx = self.ctx;
        let imports = take(&mut self.imports);
        let imports_by_id = take(&mut self.imports_by_id);
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
        ) = {
            let mut child = self.new(child_scope);
            child.imports = imports;
            child.imports_by_id = imports_by_id;
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
            )
        };
        self.storage.report_all(errors);

        self.imports = imports;
        self.imports_by_id = imports_by_id;
        self.cur_facts = cur_facts;

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
            ScopeKind::Method | ScopeKind::ArrowFn | ScopeKind::Fn => {}
            _ => {
                self.scope.return_values += child_scope.return_values;
            }
        }

        ret
    }

    fn with_ctx(&mut self, ctx: Ctx) -> WithCtx<'_, 'a, 'b> {
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

struct NoopLoader;

impl Load for NoopLoader {
    fn is_in_same_circular_group(&self, base: &Arc<PathBuf>, src: &JsWord) -> bool {
        false
    }

    fn load_non_circular_dep(
        &self,
        base: Arc<PathBuf>,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        unimplemented!()
    }

    fn load_circular_dep(
        &self,
        base: Arc<PathBuf>,
        partial: &ModuleTypeData,
        import: &DepInfo,
    ) -> Result<ModuleInfo, Error> {
        unimplemented!()
    }

    fn module_id(&self, base: &Arc<PathBuf>, src: &JsWord) -> ModuleId {
        unimplemented!()
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, modules: &mut Vec<Module>) {
        let mut counts = vec![];
        let mut items = vec![];
        for m in modules.drain(..) {
            counts.push(m.body.len());
            items.extend(m.body);
        }
        self.load_normal_imports(&items);

        let mut stmts = self.validate_stmts_with_hoisting(&mut items);
        debug_assert_eq!(stmts.len(), counts.iter().copied().sum::<usize>());
        let mut result = vec![];

        for cnt in counts {
            result.push(Module {
                // TODO
                span: DUMMY_SP,
                body: stmts.drain(0..cnt).flatten().collect(),
                // TODO
                shebang: None,
            });
        }

        *modules = result;

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, items: &mut Vec<ModuleItem>) {
        self.load_normal_imports(&items);

        let mut has_normal_export = false;
        items.iter().for_each(|item| match item {
            ModuleItem::ModuleDecl(ModuleDecl::TsExportAssignment(decl)) => {
                if self.export_equals_span.is_dummy() {
                    self.export_equals_span = decl.span;
                }
                if has_normal_export {
                    self.storage.report(Error::TS2309 { span: decl.span });
                }

                //
            }
            ModuleItem::ModuleDecl(item) => match item {
                ModuleDecl::ExportDecl(..)
                | ModuleDecl::ExportAll(..)
                | ModuleDecl::ExportDefaultDecl(..)
                | ModuleDecl::ExportDefaultExpr(..)
                | ModuleDecl::TsNamespaceExport(..) => {
                    has_normal_export = true;
                    if !self.export_equals_span.is_dummy() {
                        self.storage.report(Error::TS2309 {
                            span: self.export_equals_span,
                        });
                    }
                }
                _ => {}
            },
            _ => {}
        });

        if !self.in_declare {
            let mut visitor = AmbientFunctionHandler {
                last_ambient_name: None,
                errors: &mut self.storage,
            };

            items.visit_with(&Invalid { span: DUMMY_SP }, &mut visitor);

            if visitor.last_ambient_name.is_some() {
                visitor.errors.report(Error::TS2391 {
                    span: visitor.last_ambient_name.unwrap().span,
                })
            }
        }

        if self.is_builtin {
            items.visit_mut_children_with(self);
        } else {
            self.validate_stmts_and_collect(items);
        }

        items.retain(|item| {
            match item {
                ModuleItem::ModuleDecl(decl) => match decl {
                    ModuleDecl::ExportNamed(export @ NamedExport { src: None, .. })
                        if export.specifiers.is_empty() =>
                    {
                        return false
                    }
                    _ => {}
                },
                ModuleItem::Stmt(stmt) => match stmt {
                    Stmt::Decl(..) => {}
                    _ => return false,
                },
            }

            // Let's be conservative
            true
        });

        self.handle_pending_exports();

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, items: &mut Vec<Stmt>) {
        let mut visitor = AmbientFunctionHandler {
            last_ambient_name: None,
            errors: &mut self.storage,
        };

        items.visit_with(&Invalid { span: DUMMY_SP }, &mut visitor);

        if visitor.last_ambient_name.is_some() {
            visitor.errors.report(Error::TS2391 {
                span: visitor.last_ambient_name.unwrap().span,
            })
        }

        let mut new = Vec::with_capacity(items.len());

        for (i, item) in items.iter_mut().enumerate() {
            item.visit_mut_with(self);

            new.extend(self.prepend_stmts.drain(..));

            new.push(replace(item, Stmt::Empty(EmptyStmt { span: DUMMY_SP })));

            new.extend(self.append_stmts.drain(..));
        }

        *items = new;

        Ok(())
    }
}

/// Done
#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, d: &mut Decorator) {
        d.expr.validate_with_default(self).report(&mut self.storage);

        Ok(())
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, node: &mut TsImportEqualsDecl) {
        self.record(node);

        match node.module_ref {
            TsModuleRef::TsEntityName(ref e) => {
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
    fn validate(&mut self, decl: &mut TsNamespaceDecl) {
        todo!("namespace is not supported yet")
    }
}

#[validator]
impl Analyzer<'_, '_> {
    fn validate(&mut self, decl: &mut TsModuleDecl) {
        let span = decl.span;
        let ctxt = self.ctx.module_id;
        let global = decl.global;

        let ctx = Ctx {
            in_global: global,
            ..self.ctx
        };
        self.with_ctx(ctx).with_child(
            ScopeKind::Block,
            Default::default(),
            |child: &mut Analyzer| {
                child.ctx.in_declare = decl.declare;

                decl.visit_mut_children_with(child);

                let exports = take(&mut child.storage.take_info(ctxt));
                if !global {
                    let module = child.finalize(ty::Module { span, exports });
                    child
                        .register_type(
                            match decl.id {
                                TsModuleName::Ident(ref i) => i.into(),
                                TsModuleName::Str(ref s) => {
                                    Ident::new(s.value.clone(), s.span).into()
                                }
                            },
                            box Type::Module(module),
                        )
                        .report(&mut child.storage);
                }

                Ok(())
            },
        )
    }
}
