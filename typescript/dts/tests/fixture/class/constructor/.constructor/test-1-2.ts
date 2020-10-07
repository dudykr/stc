export function createErrorClass<T>(createImpl: (_super: any) => any): T {
    const _super = (instance: any) => {
        Error.call(instance);
        instance.stack = new Error().stack;
    };

    const ctorFunc = createImpl(_super);
    ctorFunc.prototype = Object.create(Error.prototype);
    ctorFunc.prototype.constructor = ctorFunc;
    return ctorFunc;
}

export interface UnsubscriptionError extends Error {
    readonly errors: any[];
}

export interface UnsubscriptionErrorCtor {
    new(errors: any[]): UnsubscriptionError;
}

/**
 * An error thrown when one or more errors have occurred during the
 * `unsubscribe` of a {@link Subscription}.
 */
export const UnsubscriptionError = createErrorClass(
    (_super) =>
        function UnsubscriptionErrorImpl(this: any, errors: (Error | string)[]) {
            _super(this);
            this.message = errors
                ? `${errors.length} errors occurred during unsubscription:
  ${errors.map((err, i) => `${i + 1}) ${err.toString()}`).join('\n  ')}`
                : '';
            this.name = 'UnsubscriptionError';
            this.errors = errors;
        }
);
