class Foo {
    constructor(s: string);
    constructor(n: number);
    constructor(x: any) {

    }
    constructor(x: any) {

    }
    bar1() {  /*WScript.Echo("bar1");*/ }
    bar2() {  /*WScript.Echo("bar1");*/ }
}

var f1 = new Foo("hey");
var f2 = new Foo(0);
var f3 = new Foo(f1);
var f4 = new Foo([f1,f2,f3]);

f1.bar1();
f1.bar2();
