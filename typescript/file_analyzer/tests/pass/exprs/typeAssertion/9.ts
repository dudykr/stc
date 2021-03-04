// @strictNullChecks: true

// Repro from #8513

let cond: boolean;

function f7() {
    let x: string;
    x!.slice();
}
