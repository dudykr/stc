// @declaration: true

type Constructor<T> = new (...args: any[]) => T;

class Base {
    constructor(public x: number, public y: number) { }
}


interface Printable {
    print(): void;
}

export const Printable = <T extends Constructor<Base>>(superClass: T): Constructor<Printable> & { message: string } & T =>
    class extends superClass {
        static message = "hello";
        print() {
            const output = this.x + "," + this.y;
        }
    }
