// @target: esnext, es2015

class A {
    #field = 1;
    static test(_a: A) {
        [_a.#field] = [2];
    }
}

export { }