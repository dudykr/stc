﻿interface i1 { foo: number; };
interface i2 extends i1 { foo: number; };
interface i3 extends i1 { foo: string; };
interface i4 {
 bar():any;
 bar():any;
}