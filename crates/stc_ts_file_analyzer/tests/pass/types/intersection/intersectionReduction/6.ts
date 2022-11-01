//@strict: false

type Container<Type extends string> = {
    type: Type;
}

export const f2 = (t: Container<"a"> | (Container<"b"> & Container<"c">)): Container<"a"> => t;
