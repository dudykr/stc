const a = 'a';

function f1<T extends { a: string, b: number }>(obj: T) {
    let { ...r0 } = obj;
    let { a: a1, ...r1 } = obj;
    let { a: a2, b: b2, ...r2 } = obj;
    let { 'a': a3, ...r3 } = obj;
    let { ['a']: a4, ...r4 } = obj;
    let { [a]: a5, ...r5 } = obj;

    r0;
    a1;
    r1;
    a2;
    b2;
    b2;
    r3;
    r4;
    r5;
}

export { }