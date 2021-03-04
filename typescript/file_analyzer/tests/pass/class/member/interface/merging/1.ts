interface BaseInterface {
    required: number;
    optional?: number;
}

class BaseClass {
    baseMethod() { }
    baseNumber: number;
}

interface Child extends BaseInterface {
    additional: number;
}

class Child extends BaseClass {
    classNumber: number;
    method() { }
}

// checks if properties actually were merged
var child: Child;
child.required;
child.optional;
child.additional;
child.baseNumber;
child.classNumber;
child.baseMethod();
child.method();

export { }