declare function extractIndexer<T>(p: { [n: number]: T }): T;

var a: any;

extractIndexer({
    [a]: ""
}); // Should return string

export { }