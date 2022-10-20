use rnode::define_rnode;
use swc_common::Span;

#[derive(Clone)]
pub struct Struct {
    pub span: Span,
    pub item: Vec<Box<Enum>>,
}

#[derive(Clone)]
pub enum Enum {
    StructRef(Struct),
    EnumRef(Box<Enum>),
}

define_rnode!({
    pub struct Struct {
        pub span: Span,
        pub item: Vec<Box<Enum>>,
    }
    pub enum Enum {
        StructRef(Struct),
        EnumRef(Box<Enum>),
    }
});
