
function f83<T extends { [x: string]: { x: any } }, K extends keyof T>(obj: T, key: K) {
    return obj[key]["x"] as T[K]["x"];
}
