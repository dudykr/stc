type A = ["a", number] | ["b", number] | ["c", string];
type B = "a" | "b" | "c";
declare const b: B;
const a: A = b === "a" || b === "b" ? [b, 1] : ["c", ""];

export { }