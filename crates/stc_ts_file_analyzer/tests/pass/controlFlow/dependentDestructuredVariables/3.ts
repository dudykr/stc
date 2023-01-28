//@strict: true

type Action = { kind: "A"; payload: number } | { kind: "B"; payload: string };

function f14<T extends Action>(t: T) {
  const { kind, payload } = t;
  if (kind === "A") {
    payload.toFixed();
  }
  if (kind === "B") {
    payload.toUpperCase();
  }
}
