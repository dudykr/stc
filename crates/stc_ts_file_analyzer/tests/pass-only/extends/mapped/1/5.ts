//@strict: true

export type T2 = Promise<{ x: number }> extends object & { then(onfulfilled: infer F, ...args: infer _): any } ? F : never;

declare var foo: T2
declare var foo: ((value: {
    x: number;
}) => unknown) | null | undefined
