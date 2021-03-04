// @strictNullChecks: true

// Type guards involving type parameters produce intersection types

class C {
    prop: string;
}

export function f1<T>(x: T) {
    if (x instanceof C) {
        let v1: T = x;
        let v2: C = x;
        x.prop;
    }
}