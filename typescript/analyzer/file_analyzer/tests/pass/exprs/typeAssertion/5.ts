// @strictNullChecks: true

// Repro from #8513

export function f3() {
    let x: string | number | undefined = undefined;
    x;  // undefined
    if (!x) {
        return;
    }
    x;  // string | number (guard as assertion)
}
