//@strict: true

type Action = { kind: "A"; payload: number } | { kind: "B"; payload: string };

type Animal =
  | { kind2: "swim"; payload2: string }
  | { kind2: "jump"; payload2: number };

function f10({ kind, payload }: Action, { kind2, payload2 }: Animal) {
  if (kind === "A") {
    payload.toFixed();
  }
  if (kind === "B") {
    payload.toUpperCase();
  }
}
