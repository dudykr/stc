// @strict: true
// @declaration: true

type Container<T> = {
    value: T
}

type UnwrapContainers<T extends Container<unknown>[]> = { [K in keyof T]: T[K]['value'] };
// type UnwrapContainers<T extends Container<unknown>[]> = { [K in keyof T]: T[K]['value'] };