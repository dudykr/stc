

let mutuallyRecursive = {
    a: 100,
    start() {
        return this.passthrough(this.a);
    },
    passthrough(n: number) {
        return this.sub1(n);
    },
    sub1(n: number): number {
        if (n > 0) {
            return this.passthrough(n - 1);
        }
        return n;
    }
}
interface I {
    a: number;
    start(): number;
    passthrough(n: number): number;
    sub1(n: number): number;
}
export var impl: I = mutuallyRecursive;
