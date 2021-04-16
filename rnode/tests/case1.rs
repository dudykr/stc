use rnode::define_rnode;
use swc_common::Span;

pub struct Ts {
    pub span: Span,
    pub value: Option<Box<Te>>,
}

pub enum Te {
    Unit(Option<Box<Ts>>),
}

define_rnode!({
    pub struct Ts {
        pub span: Span,
        #[arc]
        pub value: Option<Box<Te>>,
    }

    pub enum Te {
        Unit(Option<Box<Ts>>),
    }
});
