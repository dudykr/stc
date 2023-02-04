//@strict: true

// Inference to [...T] has higher priority than inference to [...T, number?]

declare function ft<T extends unknown[]>(t1: [...T], t2: [...T, number?]): T;

ft([1, 2], [1, 2, 3]);

export { }