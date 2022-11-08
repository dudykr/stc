// @strict: true

type Container<Type extends string> = {
  type: Type;
};

const f4 = (
  t: number | (Container<"b"> & { dataB: boolean } & Container<"a">)
): number => t;
