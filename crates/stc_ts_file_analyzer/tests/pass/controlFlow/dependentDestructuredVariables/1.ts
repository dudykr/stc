//@strict: true

type Action = { kind: "A"; payload: number } | { kind: "B"; payload: string };

function f13<T extends Action>({ kind, payload }: T) {
  if (kind === "A") {
    payload.toFixed();
  }
  if (kind === "B") {
    payload.toUpperCase();
  }
}
