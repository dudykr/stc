// @strictNullChecks: true

// Repro from #8513

export function f5(x: string | number) {
    if (typeof x === "string" && typeof x === "number") {
        x;  // number (guard as assertion)
    }
    else {
        x;  // string | number
    }
    x;  // string | number
}
