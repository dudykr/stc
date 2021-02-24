interface BaseInterface {
    required: number;
    optional?: number;
}

interface ChildNoBaseClass extends BaseInterface {
    additional2: string;
}
class ChildNoBaseClass {
    classString: string;
    method2() { }
}
class Grandchild extends ChildNoBaseClass {
}

var grandchild: Grandchild;
grandchild.required;
grandchild.optional;
grandchild.additional2;
grandchild.classString;
grandchild.method2();

export { }