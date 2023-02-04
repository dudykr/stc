//@strict: true

export type T2 = Promise<{ x: number }> extends object & { then(onfulfilled: infer F, ...args: infer _): any } ? // `await` only unwraps object types with a callable `then`. Non-object types are not unwrapped
    F extends ((value: infer V, ...args: infer _) => any) ? // if the argument to `then` is callable, extracts the first argument
    V :
    never : // the argument to `then` was not callable
    Promise<{ x: number }>; // non-object or non-thenable:

declare var foo: T2
declare var foo: { x: number }