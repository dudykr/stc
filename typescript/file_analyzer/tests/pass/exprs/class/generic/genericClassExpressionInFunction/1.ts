class A<T> {
    genericVar: T
}

class B2<V> {
    anon = class extends A<V> { }
}

// extends can call B
class C extends (new B2<number>().anon) {
    name: string;
}

var c = new C();
c.genericVar = 12;

export { }
