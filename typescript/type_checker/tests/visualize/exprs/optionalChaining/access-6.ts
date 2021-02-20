// GH#33744
declare const o6: <T>() => undefined | ({ x: number });
o6<number>()?.["x"];

export { }