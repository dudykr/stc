type Accessors<T> = { [K in keyof T]: (() => T[K]) | Computed<T[K]> };

type Computed<T> = {
    get?(): T;
    set?(value: T): void;
}

type VueOptions<D, M, P> = ThisType<D & M & P> & {
    computed?: Accessors<P>;
}

declare const Vue: new <D, M, P>(options: VueOptions<D, M, P>) => D & M & P;

let vue = new Vue({
    computed: {
        test(): number {
            return this.x;
        },
        hello: {
            get() {
                return "hi";
            },
            set(value: string) {
            }
        }
    }
});

vue;
vue.test;
vue.hello;

export { }