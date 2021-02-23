export interface I<T, U> {
    new(t: T, u: U);
    foo(t: T, u: U): T;
    foo2(t: T, u: U): U;
    foo3<T>(t: T, u: U): T;
    foo4<U>(t: T, u: U): T;
    foo5<T, U>(t: T, u: U): T;
    foo6<T, U>(): T;
    foo7<T, U>(u: U): T;
    foo8<T, U>(): T;
}

var i: I<string, number>;
export var r4 = i.foo('', 1); // string
export var r5 = i.foo2('', 1); // number
export var r6 = i.foo3(true, 1); // boolean
export var r7 = i.foo4('', true); // string
export var r8 = i.foo5(true, 1); // boolean
export var r9 = i.foo6(); // {}
export var r10 = i.foo7(''); // {}
export var r11 = i.foo8(); // {}