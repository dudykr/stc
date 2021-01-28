export var Symbol: { iterator: symbol };

export class C {
    [Symbol.iterator]() { }
}

(new C)[Symbol.iterator](0)