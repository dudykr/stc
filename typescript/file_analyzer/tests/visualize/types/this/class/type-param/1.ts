export class C<T extends Foo, U extends Foo> {
    t: T;
    u: U;
    r = () => {
        this.t = this.t;
        this.u = this.u;
    }
}
