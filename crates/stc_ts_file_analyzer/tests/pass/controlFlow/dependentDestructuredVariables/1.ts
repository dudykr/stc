//@strict: true

type Action = { kind: "A"; payload: number } | { kind: "B"; payload: string };

function f10({ kind, payload }: Action) {
  if (kind === "A") {
    payload.toFixed();
  }
  if (kind === "B") {
    payload.toUpperCase();
  }
}
