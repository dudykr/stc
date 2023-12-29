// @strict: true
// @declaration: true

type OldDiff<T extends keyof any, U extends keyof any> = (
    & { [P in T]: P; }
    & { [P in U]: never; }
    & { [x: string]: never; }
)[T];
interface A {
    a: 'a';
}
interface B1 extends A {
    b: 'b';
    c: OldDiff<keyof this, keyof A>;
}
type c1 = B1['c']; // 'c' | 'b'