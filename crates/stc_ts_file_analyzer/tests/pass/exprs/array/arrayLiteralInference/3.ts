//@strict: true

// Repro from #31204

declare function foo<T>(...args: T[]): T[];
export let b2: boolean[][] = foo([true], [false]);
