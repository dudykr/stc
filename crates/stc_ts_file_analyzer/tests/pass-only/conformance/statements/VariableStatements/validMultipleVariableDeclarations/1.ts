// all expected to be valid
var fn = function (s: string) { return 42; }
var fn = (s: string) => 3;
var fn: (s: string) => number;
var fn: { (s: string): number };
var fn = <(s: string) => number>null;
var fn: typeof fn;

export { }
