function foo1<T extends any>(test: T) { }
function foo2<T extends number>(test: T) { }
function foo3<T extends string>(test: T) { }
function foo4<T extends Date>(test: T) { } // valid
function foo5<T extends RegExp>(test: T) { } // valid
function foo6<T extends hm>(test: T) { }
function foo7<T extends Object>(test: T) { } // valid
function foo8<T extends "">(test: T) { }
function foo9<T extends 1 > (test: T) { }
function foo10<T extends (1)> (test: T) { }
function foo11<T extends null> (test: T) { }
function foo12<T extends undefined>(test: T) { }
function foo13<T extends void>(test: T) { }