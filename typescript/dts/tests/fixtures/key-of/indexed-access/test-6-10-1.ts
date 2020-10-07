declare type S2 = {
    a: string;
    b: string;
};

function f90<T extends S2, K extends keyof S2>(x1: S2[keyof S2], x2: T[keyof S2], x3: S2[K]) {
    x1 = x2;
    x1 = x3;
    x2 = x1;
    x2 = x3;
    x3 = x1;
    x3 = x2;
    x1.length;
    x2.length;
    x3.length;

    return { x1, x2, x3 }
}
