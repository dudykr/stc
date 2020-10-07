// @strictNullChecks: true
// @declaration: true

function verifyLibTypes<T, K extends keyof T, U>() {
    var x1: Partial<T>;
    var x1: { [P in keyof T]?: T[P] };
    var x2: Readonly<T>;
    var x2: { readonly [P in keyof T]: T[P] };
    var x3: Pick<T, K>;
    var x3: { [P in K]: T[P] };
    var x4: Record<K, U>;
    var x4: { [P in K]: U };
}

type Proxy<T> = {
    get(): T;
    set(value: T): void;
}

type Proxify<T> = {
    [P in keyof T]: Proxy<T[P]>;
}

type DeepReadonly<T> = {
    readonly [P in keyof T]: DeepReadonly<T[P]>;
};

declare function assign<T>(obj: T, props: Partial<T>): void;

declare function freeze<T>(obj: T): Readonly<T>;

declare function pick<T, K extends keyof T>(obj: T, ...keys: K[]): Pick<T, K>;

declare function mapObject<K extends string, T, U>(obj: Record<K, T>, f: (x: T) => U): Record<K, U>;

declare function proxify<T>(obj: T): Proxify<T>;

interface Point {
    x: number;
    y: number;
}

interface Shape {
    name: string;
    width: number;
    height: number;
    location: Point;
    aliased: Alias
}

interface PartialShape {
    name?: string;
    width?: number;
    height?: number;
    location?: Point;
}

type Alias = string;

interface ReadonlyShape {
    readonly name: string;
    readonly width: number;
    readonly height: number;
    readonly location: Point;
}

declare let shape: DeepReadonly<Shape>
let name = shape.name;  // string
let alias = shape.aliased;  // string
let location = shape.location;  // DeepReadonly<Point>
let x = location.x;  // number