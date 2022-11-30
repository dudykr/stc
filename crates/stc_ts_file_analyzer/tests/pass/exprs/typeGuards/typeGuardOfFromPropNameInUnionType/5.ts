class A { a: string; }
class B { b: number; }

class ClassWithUnionProp { prop: A | B; }

export function inProperty(x: ClassWithUnionProp) {
    if ("a" in x.prop) {
        let y: string = x.prop.a;
    } else {
        let z: number = x.prop.b;
    }
}
