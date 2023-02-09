// Mapped types applied to variadic tuple types

type Arrayify<T> = { [P in keyof T]: T[P][] };



// Reverse mapping through mapped type applied to variadic tuple type

declare function fm1<T extends unknown[]>(t: Arrayify<[string, number, ...T]>): T;

var tm1 = fm1([['abc'], [42], [true], ['def']]);  // [boolean, string]
var tm1: [boolean, string] = [true, 'def']
export { tm1 }