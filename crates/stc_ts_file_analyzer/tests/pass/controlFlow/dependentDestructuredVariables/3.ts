//@strict: true

type Args = ["A", number] | ["B", string];

function f40(...[kind, data]: Args) {
  if (kind === "A") {
    data.toFixed();
  }
  if (kind === "B") {
    data.toUpperCase();
  }
}
