// @target: es6
// @strict: true
declare function f(cb: (v: boolean) => [0] | PromiseLike<[0]>): void;
f(v => v ? [0] : Promise.reject());
f(async v => v ? [0] : Promise.reject());

declare function g(cb: (v: boolean) => "contextuallyTypable" | PromiseLike<"contextuallyTypable">): void;
g(v => v ? "contextuallyTypable" : Promise.reject());
g(async v => v ? "contextuallyTypable" : Promise.reject());

type MyCallback = (thing: string) => void;
declare function h(cb: (v: boolean) => MyCallback | PromiseLike<MyCallback>): void;
h(v => v ? (abc) => { } : Promise.reject());
h(async v => v ? (def) => { } : Promise.reject());
