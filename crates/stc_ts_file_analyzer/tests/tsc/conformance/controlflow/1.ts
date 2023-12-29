// @strict: true
// @allowUnreachableCode: false

type Thing = { foo: string | number, bar(): number, baz: object };

function f13(o: Thing | undefined) {
    if (o?.foo !== undefined) {
        o.foo;
    }
    if (o?.["foo"] !== undefined) {
        o["foo"];
    }
    if (o?.bar() !== undefined) {
        o.bar;
    }
    if (o?.foo != undefined) {
        o.foo;
    }
    if (o?.["foo"] != undefined) {
        o["foo"];
    }
    if (o?.bar() != undefined) {
        o.bar;
    }
}