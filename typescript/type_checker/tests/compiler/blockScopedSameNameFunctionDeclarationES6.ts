// @target: ES6
function foo(a: number) {
    if (a === 10) {
        function foo() { } // duplicate
        foo();
        foo(10); // not ok
    }
    else {
        function foo() { } // duplicate
        foo();
        foo(10);// not ok
    }
    foo(10); // not ok
    foo(); 
}
foo(10);
foo(); // not ok - needs number