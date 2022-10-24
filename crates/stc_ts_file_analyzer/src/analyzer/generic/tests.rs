use swc_common::Spanned;

use crate::analyzer::{generic::ExtendsOpts, tests::test_two};

fn test_extends(l: &str, r: &str, expected: Option<bool>, opts: ExtendsOpts) {
    test_two(l, r, |analyzer, l, r| {
        let res = analyzer.extends(l.span(), opts, &l, &r);

        assert_eq!(res, expected);
    });
}

#[test]
fn type_lit_1() {
    test_extends(
        "(x: string | undefined) => void",
        "(x?: 'hello') => void",
        Some(true),
        Default::default(),
    );
    test_extends(
        "(x?: 'hello') => void",
        "(x: string | undefined) => void",
        Some(false),
        Default::default(),
    );
}
