//@strict: true

// Repro from #40833

type Example = { foo: string, bar: number };

type PickByValueType<T, U> = {
  [K in keyof T as T[K] extends U ? K : never]: T[K]
};

type T1 = PickByValueType<Example, string>;
export const e1: T1 = {
  foo: "hello"
};
