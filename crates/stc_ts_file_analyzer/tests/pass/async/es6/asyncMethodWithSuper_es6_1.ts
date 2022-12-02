class Base {
    set setter(x: any) { }
    get getter(): any { return; }
    method(x: string): any { }

    static set setter(x: any) { }
    static get getter(): any { return; }
    static method(x: string): any { }
}

class Derived extends Base {
    a() { return async () => super.method('') }
    b() { return async () => super.getter }
    c() { return async () => super.setter = '' }
    d() { return async () => super["method"]('') }
    e() { return async () => super["getter"] }
    f() { return async () => super["setter"] = '' }
    static a() { return async () => super.method('') }
    static b() { return async () => super.getter }
    static c() { return async () => super.setter = '' }
    static d() { return async () => super["method"]('') }
    static e() { return async () => super["getter"] }
    static f() { return async () => super["setter"] = '' }
}