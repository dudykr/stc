// @strictNullChecks: true
// @declaration: true

declare function assign<T>(obj: T, props: Partial<T>): void;

interface Shape {
    name: string;
    width: number;
    height: number;
    location: Point;
}

export function f0(s1: Shape, s2: Shape) {
    assign(s1, { name: "circle" });
    assign(s2, { width: 10, height: 20 });
}
