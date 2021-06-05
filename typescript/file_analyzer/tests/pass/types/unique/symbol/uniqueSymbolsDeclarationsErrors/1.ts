interface I { readonly readonlyType: unique symbol; }

// not allowed when emitting declarations

export const obj = {
    method2(p: I["readonlyType"]): I["readonlyType"] {
        return p;
    }
};
