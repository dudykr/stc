// @target: esnext, es2022, es2015

export class Foo {
    #name;

    constructor(name) {
        this.#name = name;
    }

    getValue(x) {
        const obj = this;

        class Bar {
            #y = 100;

            [obj.#name]() {
            }
        }
    }
}

