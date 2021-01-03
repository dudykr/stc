
export class A {
    log() {

    }
}

export class B {
    b() { }
}

export declare function isA(val: A | B): val is A;

export declare function createB(): A | B;

export const v = createB();
export const a = isA(v) ? v.log() : v.b();