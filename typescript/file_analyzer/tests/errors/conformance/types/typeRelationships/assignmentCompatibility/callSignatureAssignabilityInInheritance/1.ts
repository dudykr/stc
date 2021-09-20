interface Base2 { // T
    // M's
    a: (x: number) => number;
    a2: <T>(x: T) => T;
}

// S's
interface I2 extends Base2 {
    // N's
    a: (x: number) => string; // error because base returns non-void;
}

export { }