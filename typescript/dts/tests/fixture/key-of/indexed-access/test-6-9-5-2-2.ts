
function f81<T extends { a: { x: any } }>(obj: T) {
    return obj["a"]["x"] as T["a"]["x"];
}

function f82() {
    let x1 = f81({ a: { x: "hello" } });  // string
    let x2 = f81({ a: { x: 42 } });  // number

    return { x1, x2 }
}
