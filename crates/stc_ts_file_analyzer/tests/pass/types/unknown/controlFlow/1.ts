//@strict: true

function doSomething1<T extends unknown>(value: T): T {
  if (value === undefined) {
    return value; // value: T extends unknown
  }
  value; // T & ({} | null)
  if (value === 42) {
    value; // T & {}
    throw Error("Meaning of life value");
  }
  return value; // T & ({} | null)
}

doSomething1(undefined); // undefined
