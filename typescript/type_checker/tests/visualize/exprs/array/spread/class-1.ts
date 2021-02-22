export class SymbolIterator {
    next() {
        return {
            value: Symbol(),
            done: false
        };
    }

    [Symbol.iterator]() {
        return this;
    }
}

export var array: symbol[];
export const b = array.concat([...new SymbolIterator]);
b;