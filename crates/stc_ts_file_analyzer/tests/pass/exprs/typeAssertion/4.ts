// @strictNullChecks: true

// Repro from #8513

export function f2() {
    let x: string | number | undefined = undefined;
    x;  // undefined
    if (typeof x === "string") {
        x;  // string (guard as assertion)
    }
    x;  // string | undefined
}
