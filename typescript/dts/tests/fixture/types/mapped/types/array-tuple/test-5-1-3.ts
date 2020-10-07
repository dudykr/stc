type Awaited<T> = T extends PromiseLike<infer U> ? U : T;
type Awaitified<T> = { [P in keyof T]: Awaited<T[P]> };

declare function all<T extends any[]>(...values: T): Promise<Awaitified<T>>;

function f1(a: number, b: Promise<number>, c: string[], d: Promise<string[]>) {
    let x2 = all(a, b);

    return x2
}
