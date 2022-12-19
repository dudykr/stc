// @downlevelIteration: true

// If the declaration includes an initializer expression (which is permitted only
// when the parameter list occurs in conjunction with a function body),
// the parameter type is the widened form (section 3.11) of the type of the initializer expression.

function b2(z = null, o = { x: 0, y: undefined }) { }

export default b2("string", { x: 200, y: true });

