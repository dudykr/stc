// @target: ES5
// no errors

class C {
    private static foo() { return this.foo; }
    private static bar() { this.foo(); }
}