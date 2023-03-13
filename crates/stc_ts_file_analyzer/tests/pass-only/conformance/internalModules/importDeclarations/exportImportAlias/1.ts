

module X {
    export function Y() {
        return 42;
    }

    export module Y {
        export class Point {
            constructor(public x: number, public y: number) { }
        }
    }
}

module Z {

    // 'y' should be a fundule here
    export import y = X.Y;
}

export var m: number = Z.y();

