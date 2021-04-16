class BaseClass {
    baseMethod() { }
    baseNumber: number;
}

class Child extends BaseClass {
    classNumber: number;
    method() { }
}

interface IChild extends Child {

}

/// tsc is crazy. Don't ask why.
declare var a: IChild;
a.baseNumber
a.classNumber

export { }