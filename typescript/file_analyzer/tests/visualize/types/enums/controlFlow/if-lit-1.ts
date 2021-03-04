const enum Choice { Unknown, Yes, No };

type Item =
    { kind: Choice.Yes, a: string } |
    { kind: Choice.No, b: string };

function f20(x: Item) {
    if (x.kind === Choice.Yes) {
        x
    } else {
        x
    }
}

export { }