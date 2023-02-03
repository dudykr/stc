pub mod resolver;
pub mod store;

/// A module loader.
pub trait LoadModule: 'static + Send + Sync {}
