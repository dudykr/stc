// call signatures in derived types must have the same or fewer optional parameters as the target for assignment

declare var a: (...args: number[]) => number; // ok, same number of required params
a = (x?: string) => 1; // error, incompatible type

export { }