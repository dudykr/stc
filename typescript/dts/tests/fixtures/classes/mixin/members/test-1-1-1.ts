// @declaration: true

declare class C1 {
    public a: number;
    protected b: number;
    private c: number;

    constructor(s: string);
    constructor(n: number);
}

declare class M1 {
    constructor(...args: any[]);

    p: number;
    static p: number;
}

declare const Mixed1: typeof M1 & typeof C1;

let x1 = new Mixed1("hello");
