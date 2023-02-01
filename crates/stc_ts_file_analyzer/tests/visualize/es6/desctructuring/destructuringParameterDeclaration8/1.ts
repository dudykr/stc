// explicit type annotation should cause `method` to have type 'x' | 'y'
// both inside and outside `test`.
export function test({
    nested: { p = 'c' }
}: {
    nested?: { p: 'a' | 'b' }
}) {
    p
}

test({});
