export class A {
    propA: number;
}

export class C extends A {
    propC: number;
}

export var a: A;

// The parameter index and argument index for the type guard target is matching.
// The type predicate type is assignable to the parameter type.
declare function isC_multipleParams(p1: any, p2: any): p1 is C;
if (isC_multipleParams(a, 0)) {
    a;
}