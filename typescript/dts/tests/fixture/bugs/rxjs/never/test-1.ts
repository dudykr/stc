export class Observable<T> {
    constructor(subscribe?: () => void) {
    }
}

export function noop() { }

export const NEVER = new Observable<never>(noop);

/**
 * @deprecated Deprecated in favor of using {@link NEVER} constant.
 */
export function never() {
    return NEVER;
}
