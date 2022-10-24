// @strict: true

function f2<T extends string | number | undefined, U extends string | null | undefined>(x: T & U) {
    let y2: string | null = x;       // Error
}

