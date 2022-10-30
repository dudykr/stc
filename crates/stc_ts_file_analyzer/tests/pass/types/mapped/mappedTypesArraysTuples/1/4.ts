//@strict: true

type Box<T> = { value: T };
type Boxified<T> = { [P in keyof T]: Box<T[P]> };

export function f2<T extends any[]>(a: Boxified<T>) {
    let x: Box<any> | undefined = a.pop();
    let y: Box<any>[] = a.concat(a);
}

