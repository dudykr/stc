class A { a: string; }
class B { b: number; }
class C { b: Object; }
class D { a: Date; }

export function multipleClasses(x: A | B | C | D) {
    if ("a" in x) {
        let y: string | Date = x.a;
    } else {
        let z: number | Object = x.b;
    }
}