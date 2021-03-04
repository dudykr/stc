// @target: es2015
let array: { x: number, y: string }[];
for (let { x, ...restOf } of array) {
    [x, restOf];
}

for (const norest of array.map(a => ({ ...a, x: 'a string' }))) {
    [norest.x, norest.y];
    // x is now a string. who knows why.
}

export { }