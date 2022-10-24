// @strict: true

function f2(x: { f(x: string | undefined): void }, y: { f(x?: string): void }) {
    let z = !!true ? x : y;  // { f(x?: string): void }
    z.f();
    z.f('hello');
}
