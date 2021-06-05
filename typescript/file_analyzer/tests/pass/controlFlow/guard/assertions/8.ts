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

function f5(x: string | number) {
    if (typeof x === "string" && typeof x === "number") {
        x;  // number (guard as assertion)
    }
    else {
        x;  // string | number
    }
    x;  // string | number
}
