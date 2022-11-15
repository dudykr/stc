
// Non - generic overloads where contextual typing of function arguments has errors
function fn5(f: (n: string) => void): string;
function fn5(f: (n: number) => void): number;
function fn5() { return undefined; }

var s = fn5((n) => n.substr(0));



export { }