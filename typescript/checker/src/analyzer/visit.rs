//! This module implements VisitMut for Analyzer

use super::Analyzer;
use crate::{
    validator,
    validator::{Validate, ValidateWith},
};
use rnode::Visit;
use rnode::VisitWith;
use stc_ast_rnode::RArrowExpr;
use stc_ast_rnode::RBlockStmt;
use stc_ast_rnode::RCatchClause;
use stc_ast_rnode::RClass;
use stc_ast_rnode::RClassDecl;
use stc_ast_rnode::RClassExpr;
use stc_ast_rnode::RClassMember;
use stc_ast_rnode::RClassMethod;
use stc_ast_rnode::RComputedPropName;
use stc_ast_rnode::RExportAll;
use stc_ast_rnode::RExportDecl;
use stc_ast_rnode::RExportDefaultDecl;
use stc_ast_rnode::RExportDefaultExpr;
use stc_ast_rnode::RExpr;
use stc_ast_rnode::RFnDecl;
use stc_ast_rnode::RFnExpr;
use stc_ast_rnode::RFunction;
use stc_ast_rnode::RIfStmt;
use stc_ast_rnode::RImportDecl;
use stc_ast_rnode::RModule;
use stc_ast_rnode::RModuleItem;
use stc_ast_rnode::RNamedExport;
use stc_ast_rnode::RObjectLit;
use stc_ast_rnode::RParam;
use stc_ast_rnode::RPrivateMethod;
use stc_ast_rnode::RPrivateProp;
use stc_ast_rnode::RPropName;
use stc_ast_rnode::RReturnStmt;
use stc_ast_rnode::RStmt;
use stc_ast_rnode::RSwitchStmt;
use stc_ast_rnode::RTsEnumDecl;
use stc_ast_rnode::RTsExportAssignment;
use stc_ast_rnode::RTsFnParam;
use stc_ast_rnode::RTsFnType;
use stc_ast_rnode::RTsInterfaceBody;
use stc_ast_rnode::RTsInterfaceDecl;
use stc_ast_rnode::RTsModuleDecl;
use stc_ast_rnode::RTsNamespaceDecl;
use stc_ast_rnode::RTsType;
use stc_ast_rnode::RTsTypeAliasDecl;
use stc_ast_rnode::RTsTypeElement;
use stc_ast_rnode::RVarDecl;
use stc_ast_rnode::RVarDeclarator;
use stc_ast_rnode::RWithStmt;
use stc_ast_rnode::RYieldExpr;
use swc_ecma_ast::*;

macro_rules! forward {
    ($name:ident,$T:ty) => {
        /// Delegates to `Validate<T>`
        impl Visit<$T> for Analyzer<'_, '_> {
            fn visit(&mut self, n: &$T) {
                let res = n.validate_with_default(self);
                match res {
                    // ignored
                    Ok(..) => {}
                    Err(err) => {
                        self.storage.report(err);
                    }
                }
            }
        }
    };
}

macro_rules! use_visit {
    ($T:ty) => {
        #[validator]
        impl Analyzer<'_, '_> {
            fn validate(&mut self, node: &$T) {
                node.visit_children_with(self);
                Ok(())
            }
        }
    };
}

use_visit!(RModule);

forward!(visit_expr, RExpr);
forward!(visit_block_stmt, RBlockStmt);
forward!(visit_if_stmt, RIfStmt);
forward!(visit_param, RParam);
forward!(visit_function, RFunction);
forward!(visit_fn_decl, RFnDecl);
forward!(visit_fn_expr, RFnExpr);
forward!(visit_var_decl, RVarDecl);
forward!(visit_var_declarator, RVarDeclarator);
forward!(visit_ts_interface_decl, RTsInterfaceDecl);
forward!(visit_ts_type_element, RTsTypeElement);
forward!(visit_prop_name, RPropName);
forward!(visit_computed_prop_name, RComputedPropName);
forward!(visit_class_method, RClassMethod);
forward!(visit_ts_type_alias_decl, RTsTypeAliasDecl);
forward!(visit_ts_module_decl, RTsModuleDecl);
forward!(visit_class_member, RClassMember);
forward!(visit_stmts, Vec<RStmt>);
forward!(visit_module_items, Vec<RModuleItem>);
forward!(visit_class, RClass);
forward!(visit_class_decl, RClassDecl);
forward!(visit_class_expr, RClassExpr);
forward!(visit_ts_enum_decl, RTsEnumDecl);
forward!(visit_ts_fn_param, RTsFnParam);
forward!(visit_ts_fn_type, RTsFnType);
forward!(visit_ts_type, RTsType);
forward!(visit_arrow_expr, RArrowExpr);
forward!(visit_ts_interface_body, RTsInterfaceBody);
forward!(visit_object_lit, RObjectLit);
forward!(visit_stmt, RStmt);
forward!(visit_switch_stmt, RSwitchStmt);
forward!(visit_with_stmt, RWithStmt);
forward!(visit_return_stmt, RReturnStmt);
forward!(visit_yield_expr, RYieldExpr);
forward!(visit_export_default_expr, RExportDefaultExpr);
forward!(visit_ts_export_assignment, RTsExportAssignment);
forward!(visit_export_default_decl, RExportDefaultDecl);
forward!(visit_export_decl, RExportDecl);
forward!(visit_private_method, RPrivateMethod);
forward!(visit_private_prop, RPrivateProp);
forward!(visit_import_decl, RImportDecl);
forward!(visit_export_all, RExportAll);
forward!(visit_named_export, RNamedExport);
forward!(visit_catch_clause, RCatchClause);
forward!(visit_ts_namespace_decl, RTsNamespaceDecl);
