
function f81<T extends { a: { x: any } }>(obj: T) {
    return obj["a"]["x"] as T["a"]["x"];
}
