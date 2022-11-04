//@strict: true

type ArgsUnion = [number, string] | [number, Error];
type TupleUnionFunc = (...params: ArgsUnion) => number;

const funcUnionTupleNoRest: TupleUnionFunc = (num, strOrErr) => {
  return num;
};

export { }