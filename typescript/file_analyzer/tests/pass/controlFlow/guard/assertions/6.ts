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

function f3() {
    let x: string | number | undefined = undefined;
    x;  // undefined
    if (!x) {
        return;
    }
    x;  // string | number (guard as assertion)
}
