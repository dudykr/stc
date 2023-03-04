class A {
    x: string = 'A';
    y: number = 0;
}

class B {
    x: string = 'B';
    z: boolean = true;
}

type T<X extends { x: any }> = Pick<X, 'x'>;

type C = T<A>;
type D = T<B>;

type C_extends_D = C extends D ? true : false;                                  // true
type PickA_extends_PickB = Pick<A, 'x'> extends Pick<B, 'x'> ? true : false;    // true
type TA_extends_TB = T<A> extends T<B> ? true : false;                          // should be true

declare let a: T<A>;
declare let b: T<B>;
declare let c: C;
declare let d: D;

b = a;      // should be no error
c = d;