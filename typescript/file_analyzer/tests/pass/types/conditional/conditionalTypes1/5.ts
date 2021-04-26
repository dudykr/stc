type FunctionPropertyNames<T> = { [K in keyof T]: T[K] extends Function ? K : never }[keyof T];
type FunctionProperties<T> = Pick<T, FunctionPropertyNames<T>>;
export function f7<T>(x: T, y: FunctionProperties<T>, z: NonFunctionProperties<T>) {
    y = x;
    z = x;

}
