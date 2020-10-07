export function foo() {
    return new Bar();
}

export class Bar {
    method() {
        return foo()
    }
}