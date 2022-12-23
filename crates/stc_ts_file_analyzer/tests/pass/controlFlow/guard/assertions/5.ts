//@strict: true

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

function f2() {
    let x: string | number | undefined = undefined;
    x;  // undefined
    if (typeof x === "string") {
        x;  // string (guard as assertion)
    }
    x;  // string | undefined
}
