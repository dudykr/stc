// @target: esnext
// @noEmit: true

// https://github.com/microsoft/TypeScript/pull/41094#issuecomment-716044363
declare function f(): void;


// same as above but on right of a binary expression
{
    let a: 0 | 1 = 0;
    let b: 0 | 1 | 9;
    f(), [{ [(a = 1)]: b } = [9, a] as const] = [];
    const bb: 0 = b;
}
