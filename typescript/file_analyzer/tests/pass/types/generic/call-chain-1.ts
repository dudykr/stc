declare function absorb<T>(): T;
declare const a: { m?<T>(obj: { x: T }): T } | undefined;
export const n3: number | undefined = a?.m?.({ x: 12 }); // should be ok
export const n4: number | undefined = a?.m?.({ x: absorb() }); // likewise

// Also a test showing `!` vs `?` for good measure
export let t1 = a?.m?.({ x: 12 });
t1 = a!.m!({ x: 12 });