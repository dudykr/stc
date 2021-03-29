function f3<T extends number | string[]>(arg: T) {
    return { ...arg }
}

export { }