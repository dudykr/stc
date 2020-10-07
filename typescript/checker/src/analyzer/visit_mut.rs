//! This module implements VisitMut for Analyzer

use super::Analyzer;
use crate::{
    validator,
    validator::{Validate, ValidateWith},
};
use swc_ecma_ast::*;
use swc_ecma_visit::{VisitMut, VisitMutWith};

macro_rules! forward {
    ($name:ident,$T:ty) => {
        /// Delegates to `Validate<T>`
        fn $name(&mut self, n: &mut $T) {
            let res = n.validate_with_default(self);
            match res {
                // ignored
                Ok(..) => {}
                Err(err) => {
                    self.storage.report(err);
                }
            }
        }
    };
}

macro_rules! use_visit_mut {
    ($T:ty) => {
        #[validator]
        impl Analyzer<'_, '_> {
            fn validate(&mut self, node: &mut $T) {
                node.visit_mut_children_with(self);
                Ok(())
            }
        }
    };
}

use_visit_mut!(Module);

/// All methods forward to `Validate<T>`
impl VisitMut for Analyzer<'_, '_> {
    forward!(visit_mut_expr, Expr);
    forward!(visit_mut_block_stmt, BlockStmt);
    forward!(visit_mut_if_stmt, IfStmt);
    forward!(visit_mut_param, Param);
    forward!(visit_mut_function, Function);
    forward!(visit_mut_fn_decl, FnDecl);
    forward!(visit_mut_fn_expr, FnExpr);
    forward!(visit_mut_var_decl, VarDecl);
    forward!(visit_mut_var_declarator, VarDeclarator);
    forward!(visit_mut_ts_interface_decl, TsInterfaceDecl);
    forward!(visit_mut_ts_type_element, TsTypeElement);
    forward!(visit_mut_prop_name, PropName);
    forward!(visit_mut_computed_prop_name, ComputedPropName);
    forward!(visit_mut_class_method, ClassMethod);
    forward!(visit_mut_ts_type_alias_decl, TsTypeAliasDecl);
    forward!(visit_mut_ts_module_decl, TsModuleDecl);
    forward!(visit_mut_class_member, ClassMember);
    forward!(visit_mut_stmts, Vec<Stmt>);
    forward!(visit_mut_module_items, Vec<ModuleItem>);
    forward!(visit_mut_class, Class);
    forward!(visit_mut_class_decl, ClassDecl);
    forward!(visit_mut_class_expr, ClassExpr);
    forward!(visit_mut_ts_enum_decl, TsEnumDecl);
    forward!(visit_mut_ts_fn_param, TsFnParam);
    forward!(visit_mut_ts_fn_type, TsFnType);
    forward!(visit_mut_ts_type, TsType);
    forward!(visit_mut_arrow_expr, ArrowExpr);
    forward!(visit_mut_ts_interface_body, TsInterfaceBody);
    forward!(visit_mut_object_lit, ObjectLit);
    forward!(visit_mut_stmt, Stmt);
    forward!(visit_mut_switch_stmt, SwitchStmt);
    forward!(visit_mut_with_stmt, WithStmt);
    forward!(visit_mut_return_stmt, ReturnStmt);
    forward!(visit_mut_yield_expr, YieldExpr);
    forward!(visit_mut_export_default_expr, ExportDefaultExpr);
    forward!(visit_mut_ts_export_assignment, TsExportAssignment);
    forward!(visit_mut_export_default_decl, ExportDefaultDecl);
    forward!(visit_mut_export_decl, ExportDecl);
    forward!(visit_mut_private_method, PrivateMethod);
    forward!(visit_mut_private_prop, PrivateProp);
    forward!(visit_mut_import_decl, ImportDecl);
    forward!(visit_mut_export_all, ExportAll);
    forward!(visit_mut_named_export, NamedExport);
    forward!(visit_mut_catch_clause, CatchClause);
    forward!(visit_mut_ts_namespace_decl, TsNamespaceDecl);
}
