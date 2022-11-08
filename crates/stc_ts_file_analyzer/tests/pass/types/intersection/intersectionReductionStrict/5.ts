// @strict: true

type Container<Type extends string> = {
  type: Type;
};

const f2 = (
  t: Container<"a"> | (Container<"b"> & Container<"c">)
): Container<"a"> => t;
