// @strict: true
// @declaration: true

type Container<T> = {
    value: T
}

type UnwrapContainers<T extends Container<unknown>[]> = { [K in keyof T]: T[K]['value'] };
declare function f<T extends Container<unknown>[]>(containers: [...T], callback: (...values: UnwrapContainers<T>) => void): void;