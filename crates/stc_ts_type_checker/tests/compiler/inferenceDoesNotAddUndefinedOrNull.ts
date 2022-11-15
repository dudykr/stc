// @strict: true

interface NodeArray<T extends Node> extends ReadonlyArray<T> {}

interface Node {
    forEachChild<T>(cbNode: (node: Node) => T | undefined, cbNodeArray?: (nodes: NodeArray<Node>) => T | undefined): T | undefined;
}

declare function toArray<T>(value: T | T[]): T[];
declare function toArray<T>(value: T | readonly T[]): readonly T[];

function flatMapChildren<T>(node: Node, cb: (child: Node) => readonly T[] | T | undefined): readonly T[] {
    const result: T[] = [];
    node.forEachChild(child => {
        const value = cb(child);
        if (value !== undefined) {
            result.push(...toArray(value));
        }
    });
    return result;
}

function flatMapChildren2<T>(node: Node, cb: (child: Node) => readonly T[] | T | null): readonly T[] {
    const result: T[] = [];
    node.forEachChild(child => {
        const value = cb(child);
        if (value !== null) {
            result.push(...toArray(value));
        }
    });
    return result;
}
