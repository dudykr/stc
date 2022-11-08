// @strict: true

const enum Tag1 {}
const enum Tag2 {}

declare let t1: (string & Tag1) | undefined;
declare let t2: (string & Tag2) | undefined;

t1 = t2;
t2 = t1;
