module A {
    export interface Point {
        x: number;
        y: number;
    }

    export module inA {
        export interface Point3D extends Point {
            z: number;
        }
    }
}

// no code gen expected
module B {
    import a = A;
}

// no code gen expected
module C {
    import a = A;
    import b = a.inA;
    var p: b.Point3D;
    var p = { x: 0, y: 0, z: 0 };
}

