function f5<T extends string[] | { [key: string]: any }>(arg: T) {
    return { ...arg }
}

export { }