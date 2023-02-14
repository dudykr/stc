// @strictNullChecks: true

// Repro from #8513

let cond: boolean;

export type Optional<c> = Some<c> | None;

export interface None { readonly none: string; }
export interface Some<d> { readonly some: d; }

export const none: None = { none: '' };

export function isSome<a>(value: Optional<a>): value is Some<a> {
    return 'some' in value;
}

function someFrom<b>(some: b) {
    return { some };
}

export function fn<r>(makeSome: () => r): void {
    let result: Optional<r> = null as any;
    result = someFrom(isSome(result) ? result.some : makeSome());
}

