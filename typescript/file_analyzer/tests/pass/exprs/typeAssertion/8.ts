// @strictNullChecks: true

// Repro from #8513

export function f6() {
    let x: string | undefined | null;
    x!.slice();
    x = "";
    x!.slice();
    x = undefined;
    x!.slice();
    x = null;
    x!.slice();
    x = <undefined | null>undefined;
    x!.slice();
    x = <string | undefined>"";
    x!.slice();
    x = <string | null>"";
    x!.slice();
}
