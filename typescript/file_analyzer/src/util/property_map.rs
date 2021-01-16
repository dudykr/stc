use rnode::NodeId;
use stc_ts_ast_rnode::RComputedPropName;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RPropName;
use stc_ts_types::Key;
use swc_common::TypeEq;
use swc_common::DUMMY_SP;

/// **Note**: this struct ignores span of key.
#[derive(Debug)]
pub struct PropertyMap<V> {
    inner: Vec<(Key, V)>,
}

impl<V> Default for PropertyMap<V> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<V> PropertyMap<V> {
    pub fn get_prop_name(&self, p: &Key) -> Option<&V> {
        self.inner
            .iter()
            .rev()
            .find_map(|(k, v)| if k.type_eq(&p) { Some(v) } else { None })
    }

    pub fn insert(&mut self, key: Key, v: V) {
        self.inner.push((key, v));
    }
}
