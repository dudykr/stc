//@strict: true

// Promise has previously been updated to work without arguments, but to show this fixes the issue too.

class MyPromise<X> {
    constructor(executor: (resolve: (value: X) => void) => void) {

    }
}

new MyPromise<void>(resolve => resolve()); // no error

export { }