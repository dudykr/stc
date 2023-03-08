type Any = any | string; // any
type d = (a: number) => string;
type Any2 = any | d;

declare const a: any | string;

const t1: Any = 1;
t1;

const t2 = a;
t2;

const t3: Any2 = 1;
t3;
