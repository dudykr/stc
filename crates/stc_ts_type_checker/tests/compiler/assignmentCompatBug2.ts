var b2: { b: number;} = { a: 0 }; // error

b2 = { a: 0 }; // error

b2 = {b: 0, a: 0 };

var b3: { f(n: number): number; g(s: string): number; m: number; n?: number; k?(a: any): any; };

b3 = {
    f: (n) => { return 0; },
    g: (s) => { return 0; },
    m: 0,
}; // ok

b3 = {
    f: (n) => { return 0; },
    g: (s) => { return 0; },
}; // error

b3 = {
    f: (n) => { return 0; },
    m: 0,
}; // error

b3 = {
    f: (n) => { return 0; },
    g: (s) => { return 0; },
    m: 0,
    n: 0,
    k: (a) =>{ return null; },
}; // ok

b3 = {
    f: (n) => { return 0; },
    g: (s) => { return 0; },
    n: 0,
    k: (a) =>{ return null; },
}; // error