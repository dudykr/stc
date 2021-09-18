
class Base { foo: string; }
class Derived extends Base { bar: string; }

// target type with non-generic call signatures
var a16: {
    new(x: {
        new(a: number): number;
        new(a?: number): number;
    }): number[];
    new(x: {
        new(a: boolean): boolean;
        new(a?: boolean): boolean;
    }): boolean[];
};


var b16: new <T>(x: (a: T) => T) => T[];
a16 = b16; // error
b16 = a16; // error



export { }