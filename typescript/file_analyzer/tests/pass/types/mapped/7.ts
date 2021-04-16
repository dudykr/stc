type Accessors<T> = { [K in keyof T]: (() => T[K]) | Computed<T[K]> };

type Computed<T> = {
    get?(): T;
    set?(value: T): void;
}

type VueOptions<P> = {
    computed?: Accessors<P>;
}

declare const Vue: new <P>(options: VueOptions<P>) => P;

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