// Repro from #44220

function foo<T extends { [K in keyof T as `${Extract<K, string>}y`]: number }>(foox: T) { }

const c = { x: 1 };

foo(c);
