class Base {
    #prop: number = 123;
    static method(x: Derived) {
        x.#prop
    }
}

class Derived extends Base {
    static method(x: Derived) {
        x.#prop;
    }
}

