// In methods of an object literal with a contextual type that includes some
// ThisType<T>, 'this' is of type T.

export type ObjectDescriptor<D, M> = {
    data?: D;
    methods?: M & ThisType<D & M>;  // Type of 'this' in methods is D & M
}

export declare function makeObject<D, M>(desc: ObjectDescriptor<D, M>): D & M;

export let x1 = makeObject({
    data: { x: 0, y: 0 },
    methods: {
        moveBy(dx: number, dy: number) {
            this.x += dx;  // Strongly typed this
            this.y += dy;  // Strongly typed this
        }
    }
});

