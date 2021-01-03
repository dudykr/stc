export class A {

    a() {
        return this.b()
    }


    #foo: string
    b() {
        return this.#foo
    }
}