use stc_ts_ordering::calc_eval_order;
use stc_ts_testing::{parse, parse_rnode};
use swc_common::{comments::NoopComments, FileName};

#[track_caller]
fn assert_simple(src: &str, expected: Vec<usize>) {
    let expected = expected.into_iter().map(|v| vec![v]).collect::<Vec<_>>();

    testing::run_test2(false, |cm, _handler| {
        let fm = cm.new_source_file(FileName::Anon, src.into());

        let module = parse_rnode(&fm, &NoopComments);

        let order = calc_eval_order(&module.body);

        assert_eq!(order, expected);

        Ok(())
    })
    .unwrap();
}

#[test]
fn order_1() {
    assert_simple(
        "
    function foo() {}

    function bar() {}
    ",
        vec![0, 1],
    );
}

#[test]
fn order_2() {
    assert_simple(
        "
    function foo() {
        return bar()
    }

    function bar() {}
    ",
        vec![1, 0],
    );
}

#[test]
fn order_3() {
    assert_simple(
        "
    function foo() {
        return bar()
    }

    class Bar {
    }
    ",
        vec![0, 1],
    );
}

#[test]
fn order_bfs_1() {
    assert_simple(
        "
    function foo() {
        return new Bar();
    }

    class Bar {
        method() {
            return foo()
        }
    }
    ",
        vec![0, 1],
    );
}

#[test]
fn var_1() {
    assert_simple(
        "
    function foo() {
        return new Bar();
    }

    var Bar = class {
        method() {
            return foo()
        }
    }
    ",
        vec![0, 1],
    );
}

#[test]
fn var_2() {
    assert_simple(
        "// declarations with call initializer
    const constCall = Symbol();
    let letCall = Symbol();
    var varCall = Symbol();

    // ambient declaration with type
    declare const constType: unique symbol;

    // declaration with type and call initializer
    const constTypeAndCall: unique symbol = Symbol();

    // generator function yield inference
    function* genFuncYieldConstCall() {
        yield constCall;
    }

    function* genFuncYieldLetCall() {
        yield letCall;
    }

    function* genFuncYieldVarCall() {
        yield varCall;
    }

    // generator function yield with return type query
    function* genFuncYieldConstCallWithTypeQuery(): IterableIterator<typeof
constCall> {         yield constCall;
    }
    ",
        vec![0, 1, 2, 3, 4, 5, 6, 7, 8],
    );
}

#[test]
fn dts_001() {
    assert_simple(
        "declare function f1<T>(cb: <S>(x: S) => T): T;

        declare function f2<T>(cb: <S extends number>(x: S) => T): T;

        declare function f3<T>(cb: <S extends Array<S>>(x: S) => T): T;

        let x1 = f1(x => x);  // {}
        let x2 = f2(x => x);  // number
        let x3 = f3(x => x);  // Array<any>

        // Repro from #19345

        declare const s: <R>(go: <S>(ops: { init(): S; }) => R) => R;
        const x = s(a => a.init());  // x is any, should have been {}
    ",
        vec![0, 1, 2, 3, 4, 5, 6, 7],
    );
}

#[test]
fn fn_var_1() {
    assert_simple(
        "
    const a = 1;

    export function f() {
        return a + b + c + d;
    }

    const b = 5, d = 1, c = 2;
",
        vec![0, 2, 1],
    );
}

#[test]
#[ignore = "Not implemented yet"]
fn fn_var_2() {
    assert_simple(
        "
    export function foo() {
        return bar()
    }

    function bar() {
        return baz;
    }

    const baz = 5;
    ",
        vec![2, 1, 0],
    );
}

#[test]
fn simple_01() {
    assert_simple(
        "
        export type C = B;

        export type A = 5;

        ",
        vec![0, 1],
    );
}

#[test]
#[ignore]
fn type_alias_01() {
    assert_simple(
        "
        export class MyClass {
            use(t: C) {

            }
        }

        export type C = MyClass;
        ",
        vec![1, 0],
    );
}
