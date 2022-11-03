//@strict: true

type ArgsUnion = [number, string] | [number, Error];
type TupleUnionFunc = (...params: ArgsUnion) => number;

const funcUnionTupleRest: TupleUnionFunc = (...params) => {
  const [num, strOrErr] = params;
  return num;
};


export { }