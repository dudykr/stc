class A { a: string; }
class B { b: number; }

class ClassWithUnionProp { prop: A | B; }


class NestedClassWithProp { outer: ClassWithUnionProp; }

export function innestedProperty(x: NestedClassWithProp) {
    if ("a" in x.outer.prop) {
    } else {
    }
}
