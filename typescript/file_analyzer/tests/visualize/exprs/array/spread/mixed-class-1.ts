class SymbolIterator {
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

export var array: number[] = [0, 1, ...new SymbolIterator];
