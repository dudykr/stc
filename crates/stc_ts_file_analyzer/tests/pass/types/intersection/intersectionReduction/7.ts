//@strict: false

type Container<Type extends string> = {
  type: Type;
};

export const f3 = (
  t: Container<"a"> | (Container<"b"> & { dataB: boolean } & Container<"a">),
): Container<"a"> => t;
