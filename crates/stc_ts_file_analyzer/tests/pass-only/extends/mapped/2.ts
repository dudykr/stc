type UndefinedKeys<T extends Record<string, any>> = {
    [K in keyof T]: undefined extends T[K] ? K : never
};

type MyType = { a: string, b: string | undefined }

type Result1 = UndefinedKeys<MyType>;

const a1: Result1['a'] = 'a';  // Error
const b1: Result1['b'] = 'b';

export { a1, b2 }