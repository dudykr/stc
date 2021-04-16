declare function extractIndexer<T>(p: { [n: number]: T }): T;

extractIndexer({
    ["" || 0]: ""
}); // Should return any (widened form of undefined)

export { }