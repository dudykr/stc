use rnode::define_rnode;
use swc_common::Span;

#[derive(Clone)]
pub struct Struct {
    pub span: Span,
    pub boxed: Box<Enum>,
    pub items: Vec<Enum>,
    pub vec_opt_items: Vec<Option<Enum>>,
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
        #[rc]
        pub boxed: Box<Enum>,
        #[refcell]
        pub items: Vec<Enum>,
        #[rc]
        pub vec_opt_items: Vec<Option<Enum>>,
        pub opt_item: Option<Box<Enum>>,
    }
    pub enum Enum {
        StructRef(Struct),
        EnumRef(Box<Enum>),
    }
});
