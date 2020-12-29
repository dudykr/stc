function f83<T extends { [x: string]: { x: any } }, K extends keyof T>(obj: T, key: K) {
    return obj[key]["x"] as T[K]["x"];
}

function f84() {
    let x1 = f83({ foo: { x: "hello" } }, "foo");  // string
    let x2 = f83({ bar: { x: 42 } }, "bar");  // number

    return { x1, x2 }
}