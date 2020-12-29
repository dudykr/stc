//! This module implements VisitMut for Analyzer

use super::Analyzer;
use crate::{validator, validator::ValidateWith};
use rnode::VisitMut;
use rnode::VisitMutWith;
use stc_ts_ast_rnode::RArrowExpr;
use stc_ts_ast_rnode::RBlockStmt;
use stc_ts_ast_rnode::RCatchClause;
use stc_ts_ast_rnode::RClass;
use stc_ts_ast_rnode::RClassDecl;
use stc_ts_ast_rnode::RClassExpr;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RClassMethod;
use stc_ts_ast_rnode::RComputedPropName;
use stc_ts_ast_rnode::RExportAll;
use stc_ts_ast_rnode::RExportDecl;
use stc_ts_ast_rnode::RExportDefaultDecl;
use stc_ts_ast_rnode::RExportDefaultExpr;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RFnDecl;
use stc_ts_ast_rnode::RFnExpr;
use stc_ts_ast_rnode::RFunction;
use stc_ts_ast_rnode::RIfStmt;
use stc_ts_ast_rnode::RImportDecl;
use stc_ts_ast_rnode::RModule;
use stc_ts_ast_rnode::RModuleItem;
use stc_ts_ast_rnode::RNamedExport;
use stc_ts_ast_rnode::RObjectLit;
use stc_ts_ast_rnode::RParam;
use stc_ts_ast_rnode::RPrivateMethod;
use stc_ts_ast_rnode::RPrivateProp;
use stc_ts_ast_rnode::RPropName;
use stc_ts_ast_rnode::RReturnStmt;
use stc_ts_ast_rnode::RStmt;
use stc_ts_ast_rnode::RSwitchStmt;
use stc_ts_ast_rnode::RTsEnumDecl;
use stc_ts_ast_rnode::RTsExportAssignment;
use stc_ts_ast_rnode::RTsFnParam;
use stc_ts_ast_rnode::RTsFnType;
use stc_ts_ast_rnode::RTsInterfaceBody;
use stc_ts_ast_rnode::RTsInterfaceDecl;
use stc_ts_ast_rnode::RTsModuleDecl;
use stc_ts_ast_rnode::RTsNamespaceDecl;
use stc_ts_ast_rnode::RTsType;
use stc_ts_ast_rnode::RTsTypeAliasDecl;
use stc_ts_ast_rnode::RTsTypeElement;
use stc_ts_ast_rnode::RVarDecl;
use stc_ts_ast_rnode::RVarDeclarator;
use stc_ts_ast_rnode::RWithStmt;
use stc_ts_ast_rnode::RYieldExpr;

macro_rules! forward {
    ($name:ident,$T:ty) => {
        /// Delegates to `Validate<T>`
        impl VisitMut<$T> for Analyzer<'_, '_> {
            fn visit_mut(&mut self, n: &mut $T) {
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

use_visit_mut!(RModule);

forward!(visit_mut_expr, RExpr);
forward!(visit_mut_block_stmt, RBlockStmt);
forward!(visit_mut_if_stmt, RIfStmt);
forward!(visit_mut_param, RParam);
forward!(visit_mut_function, RFunction);
forward!(visit_mut_fn_decl, RFnDecl);
forward!(visit_mut_fn_expr, RFnExpr);
forward!(visit_mut_var_decl, RVarDecl);
forward!(visit_mut_var_declarator, RVarDeclarator);
forward!(visit_mut_ts_interface_decl, RTsInterfaceDecl);
forward!(visit_mut_ts_type_element, RTsTypeElement);
forward!(visit_mut_prop_name, RPropName);
forward!(visit_mut_computed_prop_name, RComputedPropName);
forward!(visit_mut_class_method, RClassMethod);
forward!(visit_mut_ts_type_alias_decl, RTsTypeAliasDecl);
forward!(visit_mut_ts_module_decl, RTsModuleDecl);
forward!(visit_mut_class_member, RClassMember);
forward!(visit_mut_stmts, Vec<RStmt>);
forward!(visit_mut_module_items, Vec<RModuleItem>);
forward!(visit_mut_class, RClass);
forward!(visit_mut_class_decl, RClassDecl);
forward!(visit_mut_class_expr, RClassExpr);
forward!(visit_mut_ts_enum_decl, RTsEnumDecl);
forward!(visit_mut_ts_fn_param, RTsFnParam);
forward!(visit_mut_ts_fn_type, RTsFnType);
forward!(visit_mut_ts_type, RTsType);
forward!(visit_mut_arrow_expr, RArrowExpr);
forward!(visit_mut_ts_interface_body, RTsInterfaceBody);
forward!(visit_mut_object_lit, RObjectLit);
forward!(visit_mut_stmt, RStmt);
forward!(visit_mut_switch_stmt, RSwitchStmt);
forward!(visit_mut_with_stmt, RWithStmt);
forward!(visit_mut_return_stmt, RReturnStmt);
forward!(visit_mut_yield_expr, RYieldExpr);
forward!(visit_mut_export_default_expr, RExportDefaultExpr);
forward!(visit_mut_ts_export_assignment, RTsExportAssignment);
forward!(visit_mut_export_default_decl, RExportDefaultDecl);
forward!(visit_mut_export_decl, RExportDecl);
forward!(visit_mut_private_method, RPrivateMethod);
forward!(visit_mut_private_prop, RPrivateProp);
forward!(visit_mut_import_decl, RImportDecl);
forward!(visit_mut_export_all, RExportAll);
forward!(visit_mut_named_export, RNamedExport);
forward!(visit_mut_catch_clause, RCatchClause);
forward!(visit_mut_ts_namespace_decl, RTsNamespaceDecl);
