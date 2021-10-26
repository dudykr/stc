// @strictNullChecks: true

// Repro from #8513

// Type guards as assertions

export function f1() {
    let x: string | number | undefined = undefined;
    x;  // undefined
    if (x) {
        x;  // string | number (guard as assertion)
    }
    x;  // string | number | undefined
}

