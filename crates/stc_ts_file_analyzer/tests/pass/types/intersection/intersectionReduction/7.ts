//@strict: false

type Container<Type extends string> = {
    type: Type;
}

export const f4 = (t: number | (Container<"b"> & { dataB: boolean } & Container<"a">)): number => t;

