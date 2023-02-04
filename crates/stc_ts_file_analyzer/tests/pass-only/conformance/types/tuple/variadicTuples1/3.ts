//@strict: true

// Inference to [...T] has higher priority than inference to [...T, number?]

declare function ft<T extends unknown[]>(t1: [...T], t2: [...T, number?]): T;

ft(['a', 'b'], ['c', 'd', 42])

export { }