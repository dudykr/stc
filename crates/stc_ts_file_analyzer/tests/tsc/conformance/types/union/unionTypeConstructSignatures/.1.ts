var strOrNum: string | number;

export var unionWithRestParameter2: { new(a: string, ...b: number[]): string; } | { new(a: string, b: number): number };
strOrNum = new unionWithRestParameter2(); // error no call signature

