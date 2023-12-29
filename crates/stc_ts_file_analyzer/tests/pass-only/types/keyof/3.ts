// @strictNullChecks: true
// @declaration: true

class Shape {
    name: string;
    width: number;
    height: number;
    visible: boolean;
}

function getProperty<T, K extends keyof T>(obj: T, key: K) {
    return obj[key];
}

function f33<S extends Shape, K extends keyof S>(shape: S, key: K) {
    let name = getProperty(shape, "name");
    let prop = getProperty(shape, key);
    return prop;
}