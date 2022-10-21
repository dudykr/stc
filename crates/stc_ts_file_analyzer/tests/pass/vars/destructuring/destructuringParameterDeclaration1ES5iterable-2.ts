// @downlevelIteration: true
// A parameter declaration may specify either an identifier or a binding pattern.
// The identifiers specified in parameter declarations and binding patterns
// in a parameter list must be unique within that parameter list.

// If the declaration includes a type annotation, the parameter is of that type

function b2(z = null, o = { x: 0, y: undefined }) { }

export default b2("string", { x: 200, y: "string" });
