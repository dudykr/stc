export type Item =
    { kind: 0, a: string } |
    { kind: 1, b: string } |
    { kind: 2, c: string };


export function f20(x: Item) {
    switch (x.kind) {
        case 0: return x.a;
        case 1: return x.b;
        case 2: return x.c;
    }
}