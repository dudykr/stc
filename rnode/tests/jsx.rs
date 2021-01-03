use rnode::define_rnode;
use swc_common::Span;

#[derive(Clone)]
pub struct JSXStruct {
    pub span: Span,
    pub opt_item: Option<Box<Enum>>,
}

#[derive(Clone)]
pub enum Enum {
    JSXStructRef(JSXStruct),
    EnumRef(Box<Enum>),
}

define_rnode!({
    pub struct JSXStruct {
        pub span: Span,
        pub opt_item: Option<Box<Enum>>,
    }
    pub enum Enum {
        JSXStructRef(JSXStruct),
        EnumRef(Box<Enum>),
    }
});
