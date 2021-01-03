// @strict: true
// @declaration: true

type Box<T> = { value: T };
type Boxified<T> = { [P in keyof T]: Box<T[P]> };

function f2<T extends any[]>(a: Boxified<T>) {
    let x: Box<any> | undefined = a.pop();

    return x
}

