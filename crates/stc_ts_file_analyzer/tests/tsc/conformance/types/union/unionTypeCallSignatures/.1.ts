var strOrNum: string | number;

export var unionWithRestParameter2: { (a: string, ...b: number[]): string; } | { (a: string, b: number): number };
strOrNum = unionWithRestParameter2(); // error no call signature
