// @strict: true
// @declaration: true

// Last argument is contextually typed

declare function call<T extends unknown[], R>(...args: [...T, (...args: T) => R]): [T, R];

call('hello', 32, (a, b) => 42);
call(...sa, (...x) => 42);

export { }