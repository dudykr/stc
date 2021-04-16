function f4<T extends number | { [key: string]: any }>(arg: T) {
    return { ...arg }
}

export { }