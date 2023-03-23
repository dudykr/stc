declare function absorb<A>(): A;
declare const a: { m?<B>(obj: { x: B }): B } | undefined;
export const n4: number | undefined = a?.m?.({ x: absorb() }); // likewise