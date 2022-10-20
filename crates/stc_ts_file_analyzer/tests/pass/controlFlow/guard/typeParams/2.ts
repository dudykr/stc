
class C {
    prop: string;
}

function f2<T>(x: T) {
    if (typeof x === "string") {
        let v1: T = x;
        let v2: string = x;
        x.length;
    }
}

export { }