//@strict: true
function doSomething1<T extends unknown>(value: T): T {
  if (value === undefined) {
    value;
    return value; // value: T extends unknown
  }
  value;
  if (value === 42) {
    value; // T & {}
    throw Error("Meaning of life value");
  }
  value;
  throw Error("Meaning of life value");
  // return value; //T & ({} | null)
}

doSomething1(undefined);

function doSomething2(value: string | number): any {
  if (typeof value === "string") {
    return value;
  }
  value;
}
