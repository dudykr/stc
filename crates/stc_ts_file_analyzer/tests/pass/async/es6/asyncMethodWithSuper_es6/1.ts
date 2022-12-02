// @target: ES6
// @lib: esnext
// @noEmitHelpers: true

// https://github.com/microsoft/TypeScript/issues/46828
class Base {
    set setter(x: any) {}
}

class Derived extends Base {
    c() { return async () => super.setter = '' }
}
