// In methods of an object literal with a contextual type, 'this' has the
// contextual type.

export type Point = {
    x: number;
    y: number;
    z?: number;
    moveBy(dx: number, dy: number, dz?: number): void;
}

export let p1: Point = {
    x: 10,
    y: 20,
    moveBy(dx, dy, dz) {
        this.x += dx;
        this.y += dy;
        if (this.z && dz) {
            this.z += dz;
        }
    }
};
