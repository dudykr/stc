declare class Shape {
    name: string;
    width: number;
    height: number;
    visible: boolean;
}
declare function getProperty<T, K extends keyof T>(obj: T, key: K): T[K];
declare function setProperty<T, K extends keyof T>(obj: T, key: K, value: T[K]): void;

function f33<S extends Shape, K extends keyof S>(shape: S, key: K) {
    let name = getProperty(shape, "name");
    return name;
}