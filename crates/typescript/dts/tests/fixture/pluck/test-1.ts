function pluck<T, K extends keyof T>(array: T[], key: K) {
    return array.map(x => x[key]);
}