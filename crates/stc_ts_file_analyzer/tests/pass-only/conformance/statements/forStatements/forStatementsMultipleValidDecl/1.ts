//@allowUnreachableCode: true

for (var fn = function (s: string) { return 42; }; ;) { }
for (var fn = (s: string) => 3; ;) { }
for (var fn: (s: string) => number; ;) { }
for (var fn: { (s: string): number }; ;) { }
for (var fn = <(s: string) => number>null; ;) { }
for (var fn: typeof fn; ;) { }
