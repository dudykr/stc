use crate::{Id, Type, TypeParam};
use once_cell::sync::Lazy;
use std::sync::Arc;
use swc_atoms::js_word;
use swc_common::DUMMY_SP;

pub static SUPER_TYPE_MARKER: Lazy<Arc<Type>> = Lazy::new(|| {
    Arc::new(Type::Param(TypeParam {
        span: DUMMY_SP,
        name: Id::word(js_word!("")),
        constraint: None,
        default: None,
    }))
});

pub static SUB_TYPE_MARKER: Lazy<Arc<Type>> = Lazy::new(|| {
    Arc::new(Type::Param(TypeParam {
        span: DUMMY_SP,
        name: Id::word(js_word!("")),
        constraint: Some(box (**SUPER_TYPE_MARKER).clone()),
        default: None,
    }))
});
