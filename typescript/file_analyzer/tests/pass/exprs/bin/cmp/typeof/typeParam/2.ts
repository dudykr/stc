// @strictNullChecks: true

// Type guards involving type parameters produce intersection types

class C {
    prop: string;
}

export function f2<T>(x: T) {
    if (typeof x === "string") {
        let v1: T = x;
        let v2: string = x;
        x.length;
    }
}

