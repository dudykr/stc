use swc_common::Spanned;

use crate::analyzer::{assign::AssignOpts, tests::test_two};

fn test_assign(l: &str, r: &str, should_success: bool, opts: AssignOpts) {
    test_two(l, r, |analyzer, l, r| {
        let res = analyzer.assign_with_opts(&mut Default::default(), AssignOpts { span: l.span(), ..opts }, &l, &r);

        if should_success {
            assert!(res.is_ok(), "{:?}", res);
        } else {
            assert!(res.is_err(), "{:?}", res);
        }
    });
}

#[test]
fn type_lit_1() {
    test_assign(
        "(x: string | undefined) => void",
        "(x?: 'hello') => void",
        false,
        Default::default(),
    );
    test_assign("(x?: 'hello') => void", "(x: string | undefined) => void", true, Default::default());
}

#[test]
#[ignore = "Predicate validation is not implemented yet"]
fn array_filter_1() {
    test_assign(
        "(value: string, index: number, array: string[]) => value is string",
        "(x: string) => boolean;",
        false,
        Default::default(),
    );
    test_assign(
        "(x: string) => boolean;",
        "(value: string, index: number, array: string[]) => value is string",
        false,
        Default::default(),
    );
}

/// Without type predicate
#[test]
fn array_filter_2() {
    test_assign(
        "(value: string, index: number, array: string[]) => boolean",
        "(x: string) => boolean;",
        true,
        Default::default(),
    );
    test_assign(
        "(x: string) => boolean;",
        "(value: string, index: number, array: string[]) => boolean",
        false,
        Default::default(),
    );
}
