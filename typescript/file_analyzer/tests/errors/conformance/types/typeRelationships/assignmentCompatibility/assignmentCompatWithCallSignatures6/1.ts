// checking assignment compatibility relations for function types. All valid

interface A {
    a3: <T>(x: T) => void;
}

var x: A;

var b3: <T>(x: T) => T;
b3 = x.a3;


export { }