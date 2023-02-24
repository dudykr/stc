declare function absorb<T>(): T;
declare const a: { m?<T>(obj: { x: T }): T } | undefined;
export const n4: number | undefined = a?.m?.({ x: absorb() }); // likewise