declare global {
    interface SymbolConstructor {
        readonly observable: symbol;
    }
}

/** Symbol.observable or a string "@@observable". Used for interop */
export const observable = (() => typeof Symbol === 'function' && Symbol.observable || '@@observable')();
