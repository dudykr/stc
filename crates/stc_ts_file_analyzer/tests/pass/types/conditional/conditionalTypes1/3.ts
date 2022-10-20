export function f1<T>(x: T, y: NonNullable<T>) {
    x = y;
}