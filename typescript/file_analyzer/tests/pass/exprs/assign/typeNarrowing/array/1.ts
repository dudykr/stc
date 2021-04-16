type AOrArrA<T> = T | T[];
const arr: AOrArrA<{ x?: "ok" }> = [{ x: "ok" }]; // weak type
arr.push({ x: "ok" });