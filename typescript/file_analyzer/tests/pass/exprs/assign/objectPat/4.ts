

type Item = { a: string, b: number, c: boolean };

function f4<K1 extends keyof Item, K2 extends keyof Item>(obj: Item, k1: K1, k2: K2) {
    let { [k1]: a1, [k2]: a2, ...r1 } = obj;
}

export { }