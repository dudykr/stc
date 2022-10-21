// @downlevelIteration: true
// A parameter declaration may specify either an identifier or a binding pattern.
// The identifiers specified in parameter declarations and binding patterns
// in a parameter list must be unique within that parameter list.

// If the declaration includes a type annotation, the parameter is of that type
function a1([a, b, [[c]]]: [number, number, string[][]]) { }
function a2(o: { x: number, a: number }) { }
function a3({ j, k, l: { m, n }, q: [a, b, c] }: { j: number, k: string, l: { m: boolean, n: number }, q: (number | string)[] }) { };
function a4({ x, a }: { x: number, a: number }) { }

a1([1, 2, [["world"]]]);
a1([1, 2, [["world"]], 3]);

// If the declaration includes an initializer expression (which is permitted only
// when the parameter list occurs in conjunction with a function body),
// the parameter type is the widened form (section 3.11) of the type of the initializer expression.


// If the declaration specifies a binding pattern, the parameter type is the implied type of that binding pattern (section 5.1.3)
function c2({ z = 10 }) { }

export default c2({});         // Implied type is {z?: number}
