let a: 0 | 1 = 1;
const [{ [a]: b } = [a = 0, 9] as const] = [[8, 9] as const];
const bb: 0 | 8 = b;
export { }