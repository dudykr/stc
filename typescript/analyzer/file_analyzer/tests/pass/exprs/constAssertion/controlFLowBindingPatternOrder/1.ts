let a: 0 | 1 = 0;
const [{ [(a = 1)]: b } = [9, a] as const] = [[9, 8] as const];
const bb: 0 | 8 = b;

export { }