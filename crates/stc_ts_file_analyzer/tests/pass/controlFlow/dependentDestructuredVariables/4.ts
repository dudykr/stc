//@strict: true

type Func = <T extends ["a", number] | ["b", string]>(...args: T) => void;

const f60: Func = (kind, payload) => {
  if (kind === "a") {
    payload.toFixed(); // error
  }
  if (kind === "b") {
    payload.toUpperCase(); // error
  }
};
