use fxhash::FxHashMap;
use rnode::NodeId;
use stc_ts_ast_rnode::RClassMember;
use stc_ts_ast_rnode::RExpr;
use stc_ts_types::Type;

/// Stores ast mutation informations.
///
/// This includes every information required to generate correct `.d.ts` files.
///
/// Note that validations are done by the [crate::analyzer::Analyzer], so
/// implementors of this trait should not lint on something lint `implicit any`.
#[derive(Default)]
pub struct Mutations {
    pub for_pats: FxHashMap<NodeId, PatMut>,
    pub for_var_decls: FxHashMap<NodeId, VarDeclMut>,
    pub for_fns: FxHashMap<NodeId, FunctionMut>,
    pub for_classes: FxHashMap<NodeId, ClassMut>,
    pub for_class_members: FxHashMap<NodeId, ClassMemberMut>,
    pub for_class_props: FxHashMap<NodeId, ClassPropMut>,
}

#[derive(Default)]
pub struct PatMut {
    /// None: No change
    pub optional: Option<bool>,
    pub ty: Option<Box<Type>>,
}

#[derive(Default)]
pub struct VarDeclMut {
    pub remove_init: bool,
}

#[derive(Default)]
pub struct FunctionMut {
    pub ret_ty: Option<Box<Type>>,
}

#[derive(Default)]
pub struct ClassMut {
    pub super_class: Option<Box<RExpr>>,
    pub additional_members: Vec<RClassMember>,
}

#[derive(Default)]
pub struct ClassMemberMut {
    pub remove: bool,
}

#[derive(Default)]
pub struct ClassPropMut {
    pub ty: Option<Box<Type>>,
}
