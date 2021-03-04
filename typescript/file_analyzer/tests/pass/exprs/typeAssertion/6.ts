// @strictNullChecks: true

// Repro from #8513


export function f4() {
    let x: string | number | undefined = undefined;
    x;  // undefined
    if (typeof x === "boolean") {
        x;  // nothing (boolean not in declared type)
    }
    x;  // undefined
}
