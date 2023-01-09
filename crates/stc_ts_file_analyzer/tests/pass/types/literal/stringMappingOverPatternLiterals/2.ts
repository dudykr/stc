// string mapping over non-string pattern literals is preserved

type NonStringPat = Uppercase<`aA${number}${bigint}${boolean}`>;
type EquivalentNonStringPat = `AA${Uppercase<`${number}`>}${Uppercase<`${bigint}`>}${Uppercase<`${boolean}`>}`;

export function f4(x1: NonStringPat, x2: EquivalentNonStringPat) {
    // Should both work
    x2 = x1;
}
