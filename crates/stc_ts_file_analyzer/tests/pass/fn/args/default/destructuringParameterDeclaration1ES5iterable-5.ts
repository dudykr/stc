// @downlevelIteration: true
// A parameter declaration may specify either an identifier or a binding pattern.
// The identifiers specified in parameter declarations and binding patterns
// in a parameter list must be unique within that parameter list.

// If the declaration specifies a binding pattern, the parameter type is the implied type of that binding pattern (section 5.1.3)
function c2({ z = 10 }) { }

export default c2({});         // Implied type is {z?: number}
