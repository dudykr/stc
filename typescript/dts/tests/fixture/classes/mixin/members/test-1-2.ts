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

declare class M2 {
    constructor(...args: any[]);

    f(): number;

    static f(): number;
}

declare const Mixed1: typeof M1 & typeof C1;
declare const Mixed2: typeof C1 & typeof M1;
declare const Mixed3: typeof M2 & typeof M1 & typeof C1;
declare const Mixed4: typeof C1 & typeof M1 & typeof M2;
declare const Mixed5: typeof M1 & typeof M2;

let x = new Mixed1("hello");
let a = xx.a;
let p = x.p;
let mp = Mixed1.p;

class C2 extends Mixed1 {
    constructor() {
        super("hello");
        this.a;
        this.b;
        this.p;
    }
}

class C3 extends Mixed3 {
    constructor() {
        super(42);
        this.a;
        this.b;
        this.p;
        this.f();
    }

    f() {
        return super.f();
    }
}
