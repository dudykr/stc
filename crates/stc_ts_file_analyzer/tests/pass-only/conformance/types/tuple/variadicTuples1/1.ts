//@strict: true

declare function f22<T extends unknown[] = []>(args: [...T]): T;

function f23<U extends string[]>(args: [...U, number]) {
    let v2 = f22(["foo", "bar"]);  // [string, string]
}

export { }

// Mapped types applied to variadic tuple types

type Arrayify<T> = { [P in keyof T]: T[P][] };



// Reverse mapping through mapped type applied to variadic tuple type

declare function fm1<T extends unknown[]>(t: Arrayify<[string, number, ...T]>): T;

var tm1 = fm1([['abc'], [42], [true], ['def']]);  // [boolean, string]
var tm1: [boolean, string] = [true, 'def']
export { tm1 }