use rnode::define_rnode;
use swc_common::Span;

#[derive(Clone)]
pub struct Struct {
    pub span: Span,
    pub opt_item: Option<Box<Enum>>,
}

#[derive(Clone)]
pub enum Enum {
    StructRef(Struct),
    EnumRef(Box<Enum>),
}

define_rnode!({
    pub struct Struct {
        pub span: Span,
        pub opt_item: Option<Box<Enum>>,
    }
    pub enum Enum {
        StructRef(Struct),
        EnumRef(Box<Enum>),
    }
});
