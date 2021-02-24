let C = class {
    foo() {
        return new C();
    }
};
export let x = (new C).foo();