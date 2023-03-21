
type T = { isA: true, a: 1 } | { isA: false, b: true }


declare var t: T;
declare var f: () => T;

{
    const { isA } = t

    if (isA) {
        t.a
    }
}
{
    const tmp = { obj: f() };
    const { isA } = tmp?.obj

    if (isA) {
        tmp.obj.a
    }
}