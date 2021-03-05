export function foo<T extends "foo">(f: (x: T) => T) {
    return f;
}

export let f = foo((y: "foo" | "bar") => y === "foo" ? y : "foo");
export let fResult = f("foo");