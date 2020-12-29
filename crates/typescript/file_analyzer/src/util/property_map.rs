use stc_ts_ast_rnode::RComputedPropName;
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RPropName;
use swc_common::EqIgnoreSpan;
use swc_common::DUMMY_SP;

/// **Note**: this struct ignores span of key.
#[derive(Debug)]
pub struct PropertyMap<V> {
    inner: Vec<(RPropName, V)>,
}

impl<V> Default for PropertyMap<V> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<V> PropertyMap<V> {
    pub fn get(&self, expr: &RExpr) -> Option<&V> {
        let expr = RPropName::Computed(RComputedPropName {
            span: DUMMY_SP,
            expr: box expr.clone(),
        });

        self.inner.iter().rev().find_map(|(k, v)| {
            if k.eq_ignore_span(&expr) {
                Some(v)
            } else {
                None
            }
        })
    }

    pub fn get_prop_name(&self, p: &RPropName) -> Option<&V> {
        self.inner
            .iter()
            .rev()
            .find_map(|(k, v)| if k.eq_ignore_span(&p) { Some(v) } else { None })
    }

    pub fn insert(&mut self, key: RPropName, v: V) {
        self.inner.push((key, v));
    }
}
