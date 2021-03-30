declare function extractIndexer<T>(p: { [n: number]: T }): T;

enum E { x }

extractIndexer({
    [E.x]: ""
}); // Should return string

export { }