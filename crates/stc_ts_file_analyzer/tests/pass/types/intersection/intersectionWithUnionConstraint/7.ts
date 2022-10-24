// @strict: true

function f3<T extends string | number | undefined>(x: T & (number | object | undefined)) {
    const y: number | undefined = x;
}

