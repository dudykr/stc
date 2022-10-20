#![allow(incomplete_features)]
#![feature(specialization)]

use stc_visit::*;
#[derive(Debug, PartialEq, Eq, Visit)]
struct FoldableStruct {
    s: Box<FoldableEnum>,
}

#[derive(Debug, PartialEq, Eq, Visit)]
enum FoldableEnum {
    Struct(Box<FoldableStruct>),
    Enum(Box<FoldableEnum>),
    Unit,
}

struct Visitor {}

impl Fold<FoldableEnum> for Visitor {
    fn fold(&mut self, val: FoldableEnum) -> FoldableEnum {
        FoldableEnum::Enum(Box::new(val))
    }
}

#[test]
fn test() {
    let mut v = Visitor {};

    let s = FoldableStruct {
        s: Box::new(FoldableEnum::Unit),
    };

    let s = s.fold_with(&mut v);

    assert_eq!(
        s,
        FoldableStruct {
            s: Box::new(FoldableEnum::Enum(Box::new(FoldableEnum::Unit)))
        }
    )
}
