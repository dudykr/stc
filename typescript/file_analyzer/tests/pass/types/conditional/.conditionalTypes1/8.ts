type RecursivePartial<T> = {
    [P in keyof T]?: T[P] extends Array<any> ? { [index: number]: RecursivePartial<T[P][0]> } :
    T[P] extends object ? RecursivePartial<T[P]> : T[P];
};

declare function assign<T>(o: T, a: RecursivePartial<T>): void;

export var a = { o: 1, b: 2, c: [{ a: 1, c: '213' }] }
assign(a, { o: 2, c: { 0: { a: 2, c: '213123' } } })