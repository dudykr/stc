// @strictNullChecks: true
// @declaration: true

function verifyLibTypes<T1, K extends keyof T1, U>() {
    var x1: Partial<T1>;
    var x1: { [P in keyof T1]?: T1[P] };
    var x2: Readonly<T1>;
    var x2: { readonly [P in keyof T1]: T1[P] };
    var x3: Pick<T1, K>;
    var x3: { [P in K]: T1[P] };
    var x4: Record<K, U>;
    var x4: { [P in K]: U };
}

type Proxy<T2> = {
    get(): T2;
    set(value: T2): void;
}

type Proxify<T3> = {
    [P in keyof T3]: Proxy<T3[P]>;
}

type DeepReadonly<T4> = {
    readonly [P in keyof T4]: DeepReadonly<T4[P]>;
};

declare function assign<T5>(obj: T5, props: Partial<T5>): void;

declare function freeze<T6>(obj: T6): Readonly<T6>;

declare function pick<T7, K extends keyof T7>(obj: T7, ...keys: K[]): Pick<T7, K>;

declare function mapObject<K extends string, T8, U>(obj: Record<K, T8>, f: (x: T8) => U): Record<K, U>;

declare function proxify<T9>(obj: T9): Proxify<T9>;

interface Point {
    x: number;
    y: number;
}

interface Shape {
    name: string;
    width: number;
    height: number;
    location: Point;
}

interface PartialShape {
    name?: string;
    width?: number;
    height?: number;
    location?: Point;
}

interface ReadonlyShape {
    readonly name: string;
    readonly width: number;
    readonly height: number;
    readonly location: Point;
}

function f0(s1: Shape, s2: Shape) {
    assign(s1, { name: "circle" });
    assign(s2, { width: 10, height: 20 });
}

function f1(shape: Shape) {
    var frozen: ReadonlyShape;
    var frozen: Readonly<Shape>;
    var frozen = freeze(shape);
}

function f2(shape: Shape) {
    var partial: PartialShape;
    var partial: Partial<Shape>;
    var partial: Partial<Shape> = {};
}

function f3(shape: Shape) {
    const x = pick(shape, "name", "location");  // { name: string, location: Point }
}

function f4() {
    const rec = { foo: "hello", bar: "world", baz: "bye" };
    const lengths = mapObject(rec, s => s.length);  // { foo: number, bar: number, baz: number }
}

function f5(shape: Shape) {
    const p = proxify(shape);
    let name = p.name.get();
    p.width.set(42);
}

function f6(shape: DeepReadonly<Shape>) {
    let name = shape.name;  // string
    let location = shape.location;  // DeepReadonly<Point>
    let x = location.x;  // number
}