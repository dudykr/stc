type Accessors<T> = { [K in keyof T]: (() => T[K]) | Computed<T[K]> };

type Computed<T> = {
    get?(): T;
    set?(value: T): void;
}

declare const Vue: new <P>(options: Accessors<P>) => P;

let vue = new Vue({
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
});

vue;
vue.test;
vue.hello;

export { }