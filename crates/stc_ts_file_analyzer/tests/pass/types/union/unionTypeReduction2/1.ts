// @strict: true

function f1(x: { f(): void }, y: { f(x?: string): void }) {
    let z = !!true ? x : y;  // { f(x?: string): void }
    z.f();
    z.f('hello');
}
