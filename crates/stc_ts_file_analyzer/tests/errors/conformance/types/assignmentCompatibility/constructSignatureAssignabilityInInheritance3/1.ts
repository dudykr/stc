// checking subtype relations for function types as it relates to contextual signature instantiation
// error cases

class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }
class OtherDerived extends Base { bing: string; }


// base type with non-generic call signatures
interface A {
    a2: new (x: number) => string[];
    a7: new (x: (arg: Base) => Derived) => (r: Base) => Derived2;
    a8: new (x: (arg: Base) => Derived, y: (arg2: Base) => Derived) => (r: Base) => Derived;
    a10: new (...x: Base[]) => Base;
    a11: new (x: { foo: string }, y: { foo: string; bar: string }) => Base;
    a12: new (x: Array<Base>, y: Array<Derived2>) => Array<Derived>;
    a14: {
        new(x: number): number[];
        new(x: string): string[];
    };
    a15: new (x: { a: string; b: number }) => number;
    a16: {
        // type of parameter is overload set which means we can't do inference based on this type
        new(x: {
            new(a: number): number;
            new(a?: number): number;
        }): number[];
        new(x: {
            new(a: boolean): boolean;
            new(a?: boolean): boolean;
        }): boolean[];
    };
}


interface I3 extends A {
    // valid, no inferences for V so it defaults to Derived2
    a7: new <T extends Base, U extends Derived, V extends Derived2>(x: (arg: T) => U) => (r: T) => V;
}

export { }

