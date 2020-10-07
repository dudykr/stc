// @strictNullChecks: true
// @declaration: true

type DeepReadonly<T> = {
    readonly [P in keyof T]: DeepReadonly<T[P]>;
};

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

declare let shape: DeepReadonly<Shape>
let location = shape.location;
let x = location.x;