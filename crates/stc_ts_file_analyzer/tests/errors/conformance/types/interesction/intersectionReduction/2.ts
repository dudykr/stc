// @strict: false

const x1 = { a: "foo", b: 42 };
const x2 = { a: "foo", b: true };

declare let k: "a" | "b";

x1[k] = "bar" as any; // Error
x2[k] = "bar" as any; // Error
