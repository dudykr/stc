

declare function g5<T extends number>(x: T, y: T): T[];

export const x6 = g5(1, 2);  // Type (1 | 2)[]
