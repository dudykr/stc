//@strict: true

type Action = { kind: "A"; payload: number } | { kind: "B"; payload: string };

function f10(foo: Action) {
  const { kind, payload } = foo;
  if (kind === "A") {
    payload.toFixed();
  }
  if (kind === "B") {
    payload.toUpperCase();
  }
}
