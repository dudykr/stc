//@strict:false
type FuncType = (x: <T>(p: T) => T) => typeof x;

export var lambda1: FuncType = x => { x<number>(undefined); return x; };
export var lambda2: FuncType = (x => { x<number>(undefined); return x; });

