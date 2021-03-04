type Result<T> = { error?: undefined, value: T } | { error: Error };

export function f15(x: Result<number>) {
    if (!x.error) {
        x.value;
    }
    else {
        x.error.message;
    }
}
