declare class Shape {
    name: string;
    width: number;
    height: number;
    visible: boolean;
}
declare class TaggedShape extends Shape {
    tag: string;
}
declare class Item {
    name: string;
    price: number;
}
declare class Options {
    visible: "yes" | "no";
}
declare type Dictionary<T> = {
    [x: string]: T;
};
declare type NumericallyIndexed<T> = {
    [x: number]: T;
};
declare const enum E {
    A = 0,
    B = 1,
    C = 2
}
declare type KeyOf<T> = keyof T;
declare type NAME = "name";
declare type WIDTH_OR_HEIGHT = "width" | "height";
declare let cond: boolean;
declare function getProperty<T, K extends keyof T>(obj: T, key: K): T[K];
declare function setProperty<T, K extends keyof T>(obj: T, key: K, value: T[K]): void;
declare class Component<PropType> {
    props: PropType;
    getProperty<K extends keyof PropType>(key: K): PropType[K];
    setProperty<K extends keyof PropType>(key: K, value: PropType[K]): void;
}
declare function pluck<T, K extends keyof T>(array: T[], key: K): T[K][];
declare class C {
    x: string;
    protected y: string;
    private z;
}
declare class C1 {
    x: number;
    get<K extends keyof this>(key: K): this[K];
    set<K extends keyof this>(key: K, value: this[K]): void;
    foo(): void;
}
declare type S2 = {
    a: string;
    b: string;
};

function f91<T, K extends keyof T>(x: T, y: T[keyof T], z: T[K]) {
    let a: {};
    a = x;
    a = y;
    a = z;
    return { a, x, y, z }
}
