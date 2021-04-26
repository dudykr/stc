export function f2<T extends string | undefined>(x: T, y: NonNullable<T>) {
    x = y;
    let s2: string = y;
}