//////////////////////////////////////

module m2 {
    interface Promise<T> {
        then<U>(cb: (x: T) => Promise<U>): Promise<U>;
    }

    declare function testFunction(n: number): Promise<number>;
    declare function testFunction(s: string): Promise<string>;

    var numPromise: Promise<number>;
    var newPromise = numPromise.then(testFunction);
}

