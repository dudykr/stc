// @strictNullChecks: true

// Repro from #8513

let cond: boolean;



export function foo1() {
    let x: string | number | boolean = 0;
    x;  // number
    while (cond) {
        x;  // number, then string | number
        x = typeof x === "string" ? x.slice() : "abc";
        x;  // string
    }
    x;
}
