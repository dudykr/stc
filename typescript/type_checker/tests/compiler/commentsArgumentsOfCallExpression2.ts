﻿function foo(/*c1*/ x: any, /*d1*/ y: any,/*e1*/w?: any) { }
var a, b: any;
foo(/*c2*/ 1, /*d2*/ 1 + 2, /*e1*/ a + b);
foo(/*c3*/ function () { }, /*d2*/() => { }, /*e2*/ a + /*e3*/ b);
foo(/*c3*/ function () { }, /*d3*/() => { }, /*e3*/(a + b));
foo(
    /*c4*/ function () { },
    /*d4*/() => { },
    /*e4*/
    /*e5*/ "hello");