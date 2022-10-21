// @downlevelIteration: true
// A parameter declaration may specify either an identifier or a binding pattern.
// The identifiers specified in parameter declarations and binding patterns
// in a parameter list must be unique within that parameter list.

// If the declaration includes a type annotation, the parameter is of that type

// If the declaration includes an initializer expression (which is permitted only
// when the parameter list occurs in conjunction with a function body),
// the parameter type is the widened form (section 3.11) of the type of the initializer expression.

function b1(z = [undefined, null]) { };


export default b1([1, 2, 3]);  // z is widen to the type any[]
