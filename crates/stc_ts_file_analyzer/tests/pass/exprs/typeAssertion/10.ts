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

export function fn<r>(makeSome: () => r): void {
    let result: Optional<r> = none;
    result;  // None
    while (cond) {
        result;  // Some<r> | None
        result = someFrom(isSome(result) ? result.some : makeSome());
        result;  // Some<r>
    }
}
