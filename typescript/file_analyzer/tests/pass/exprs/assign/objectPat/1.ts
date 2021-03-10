const a = 'a';

function f1<T extends { a: string, b: number }>(obj: T) {
    let { ...r0 } = obj;
    let { a: a1, ...r1 } = obj;
    let { a: a2, b: b2, ...r2 } = obj;
    let { 'a': a3, ...r3 } = obj;
    let { ['a']: a4, ...r4 } = obj;
    let { [a]: a5, ...r5 } = obj;
}

export { }