// @strict: true

// Extract<T, Function> is a T that is known to be a Function
function isFunction<T>(value: T): value is Extract<T, Function> {
    return typeof value === "function";
}

// Repros from #22860

class Opt<T> {
    toVector(): Vector<T> {
        return <any>undefined;
    }
}

interface Seq<T> {
    tail(): Opt<Seq<T>>;
}

class Vector<T> implements Seq<T> {
    tail(): Opt<Vector<T>> {
        return <any>undefined;
    }
    partition2<U extends T>(predicate: (v: T) => v is U): [Vector<U>, Vector<Exclude<T, U>>];
    partition2(predicate: (x: T) => boolean): [Vector<T>, Vector<T>];
    partition2<U extends T>(predicate: (v: T) => boolean): [Vector<U>, Vector<any>] {
        return <any>undefined;
    }
}

interface A1<T> {
    bat: B1<A1<T>>;
}

interface B1<T> extends A1<T> {
    bat: B1<B1<T>>;
    boom: T extends any ? true : true
}

// Repro from #22899

declare function toString1(value: object | Function): string;
declare function toString2(value: Function): string;

export function foo<T>(value: T) {
    if (isFunction(value)) {
        toString1(value);
        toString2(value);
    }
}
