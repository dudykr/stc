// @strictNullChecks: true

// Repro from #8513

let cond: boolean;

export type Optional<a> = Some<a> | None;

export interface None { readonly none: string; }
export interface Some<a> { readonly some: a; }

export const none: None = { none: '' };

export function isSome<a>(value: Optional<a>): value is Some<a> {
    return 'some' in value;
}

function someFrom<a>(some: a) {
    return { some };
}


function f6() {
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
