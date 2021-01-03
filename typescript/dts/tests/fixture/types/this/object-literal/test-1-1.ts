// In methods of an object literal with no contextual type, 'this' has the type
// of the object literal.

export let obj1 = {
    a: 1
};

export const a = obj1.a;