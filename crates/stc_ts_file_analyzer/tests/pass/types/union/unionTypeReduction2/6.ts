//@strict: true

type A = {
    f(): void;
}

type B = {
    f(x?: string): void;
    g(): void;
}

export function f11(a: A, b: B) {
    let z = !!true ? a : b;  // A | B
    z.f();
    z.f('hello');
}

