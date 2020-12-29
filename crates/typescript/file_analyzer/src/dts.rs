use fxhash::FxHashMap;
use rnode::NodeId;
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
}

#[derive(Default)]
pub struct PatMut {
    pub ty: Option<Box<Type>>,
}

#[derive(Default)]
pub struct VarDeclMut {
    pub remove_init: bool,
}
