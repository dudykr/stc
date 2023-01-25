interface FConstructor {
  new (): any;
}
interface F {
  foo: string;
  bar: number;
}
declare var F: FConstructor;

var obj11: F | string;
if (obj11 instanceof F) {
  // can't type narrowing, construct signature returns any.
  obj11; // F | string Because F constructor is any
}
