// @downlevelIteration: true

// If the declaration includes an initializer expression (which is permitted only
// when the parameter list occurs in conjunction with a function body),
// the parameter type is the widened form (section 3.11) of the type of the initializer expression.

function b7([[a], b, [[c, d]]] = [[undefined], undefined, [[undefined, undefined]]]) { }

export default b7([["string"], 1, [[true, false]]]);    // Shouldn't be an error
