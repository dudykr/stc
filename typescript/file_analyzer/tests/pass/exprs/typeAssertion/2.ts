// @strictNullChecks: true

// Repro from #8513

let cond: boolean;

export function foo2() {
    let x: string | number | boolean = 0;
    x;  // number
    while (cond) {
        x;  // number, then string | number
        if (typeof x === "string") {
            x = x.slice();
        }
        else {
            x = "abc";
        }
        x;  // string
    }
    x;
}
