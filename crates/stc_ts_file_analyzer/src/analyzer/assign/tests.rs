use crate::analyzer::{assign::AssignOpts, tests::test_two};
use swc_common::Spanned;

fn test(l: &str, r: &str, opts: AssignOpts, should_success: bool) {
    test_two(l, r, |analyzer, l, r| {
        let res = analyzer.assign_with_opts(&mut Default::default(), AssignOpts { span: l.span(), ..opts }, &l, &r);

        if should_success {
            assert!(res.is_ok(), "{:?}", res);
        } else {
            assert!(res.is_err(), "{:?}", res);
        }
    });
}
