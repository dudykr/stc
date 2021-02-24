
declare const o2: undefined | { b: { c: string } };
o2?.["b"].c;
o2?.b["c"];

// GH#36031
o2?.["b"]!.c;
o2?.["b"]!["c"];
o2?.["b"]!.c!;
o2?.["b"]!["c"]!;

export { }