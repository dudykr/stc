declare function extractIndexer<T>(p: { [n: number]: T }): T;

const x = extractIndexer({
    ["" || 0]: ""
}); // Should return any (widened form of undefined)

export { x }