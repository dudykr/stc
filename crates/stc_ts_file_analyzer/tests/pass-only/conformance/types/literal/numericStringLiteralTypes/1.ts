//@strict: true





type Container<T> = {
    value: T
}

type UnwrapContainers<T extends Container<unknown>[]> = { [K in keyof T]: T[K]['value'] };

declare function createContainer<T extends unknown>(value: T): Container<T>;

declare function f<T extends Container<unknown>[]>(containers: [...T], callback: (...values: UnwrapContainers<T>) => void): void;

const container1 = createContainer('hi')
const container2 = createContainer(2)

f([container1, container2], (value1, value2) => {
    value1;  // string
    value2;  // number
});

export { }
