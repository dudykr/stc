declare const Vue: new <D, M, P>(options: VueOptions<D, M, P>) => D & M & P;

let vue = new Vue({
    data: () => ({ x: 1, y: 2 }),
    methods: {
        f(x: string) {
            return this.x;
        }
    },
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
vue.x;
vue.f("abc");
vue.test;
vue.hello;


export { }