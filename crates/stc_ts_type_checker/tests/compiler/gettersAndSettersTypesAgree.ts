class C {
    public get Foo() { return "foo";} // ok
    public set Foo(foo) {} // ok - type inferred from getter return statement

    public get Bar() { return "foo";} // ok
    public set Bar(bar:string) {} // ok - type must be declared
}

var o1 = {get Foo(){return 0;}, set Foo(val){}}; // ok - types agree (inference)
var o2 = {get Foo(){return 0;}, set Foo(val:number){}}; // ok - types agree