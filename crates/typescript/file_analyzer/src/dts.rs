/// Stores ast mutation informations.
///
/// This includes every information required to generate correct `.d.ts` files.
///
/// Note that validations are done by the [crate::analyzer::Analyzer], so
/// implementors of this trait should not lint on something lint `implicit any`.
pub trait MutationStore {}

pub enum Mutation {}
