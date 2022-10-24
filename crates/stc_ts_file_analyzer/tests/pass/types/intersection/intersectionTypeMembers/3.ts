// An intersection type has those members that are present in any of its constituent types,
// with types that are intersections of the respective members in the constituent types

interface D {
    nested: { doublyNested: { d: string; }, different: { e: number } };
}
interface E {
    nested: { doublyNested: { f: string; }, other: { g: number } };
}
const de: D & E = {
    nested: {
        doublyNested: {
            d: 'yes',
            f: 'no'
        },
        different: { e: 12 },
        other: { g: 101 }
    }
}

