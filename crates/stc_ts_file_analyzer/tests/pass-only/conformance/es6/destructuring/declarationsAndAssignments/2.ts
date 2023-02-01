
function f14([a = 1, [b = "hello", { x, y: c = false }]]) {
    var a: number;
    var b: string;
    var c: boolean;
}
f14([2, ["abc", { x: 0 }]]);

export { }

