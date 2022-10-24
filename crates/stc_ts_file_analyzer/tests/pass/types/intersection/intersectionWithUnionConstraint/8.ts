// @strict: true

function f4<T extends string | number>(x: T & (number | object)) {
    const y: number = x;
}

