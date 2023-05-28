// checking subtype relations for function types as it relates to contextual signature instantiation
// error cases

// base type has generic call signature
interface C {
    a2: new <T>(x: T) => string[];
}

export interface I7 extends C {
    a2: new <T>(x: T) => T[]; // error
}
