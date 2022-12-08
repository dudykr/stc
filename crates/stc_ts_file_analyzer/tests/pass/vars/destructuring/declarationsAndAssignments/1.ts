export function f21(v: [number, string, boolean]) {
    var x: number;
    var y: string;
    var z: boolean;
    var a0: [number, string, boolean];
    var a1: [string, boolean];
    var a2: [boolean];
    var a3: [];
    var [...a0] = v;
    var [x, ...a1] = v;
    var [x, y, ...a2] = v;
    var [x, y, z, ...a3] = v;
    [...a0] = v;
    [x, ...a1] = v;
    [x, y, ...a2] = v;
    [x, y, z, ...a3] = v;
}
