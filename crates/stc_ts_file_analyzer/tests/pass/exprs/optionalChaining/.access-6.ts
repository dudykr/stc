// GH#33744
declare const o6: <T>() => undefined | ({ x: number });
const x = o6<number>()?.["x"];

export { x }