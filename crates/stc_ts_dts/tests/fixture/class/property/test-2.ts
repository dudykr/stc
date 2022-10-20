export class Foo {
    static a = 5;

    b = Foo.a
    c = this.b
}