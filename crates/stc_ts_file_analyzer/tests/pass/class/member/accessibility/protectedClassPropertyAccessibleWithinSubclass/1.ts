class B {
    protected static x: string;
}

class C extends B {
    protected static get y() { return this.x; }
}