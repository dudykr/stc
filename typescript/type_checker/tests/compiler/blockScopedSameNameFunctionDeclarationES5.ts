// @target: ES5
function foo(a: number) {
    if (a === 1) {
        function foo() { } // duplicate function
        foo(); 
        foo(10); // not ok
    }
    else {
        function foo() { } // duplicate function
        foo(); 
        foo(10); // not ok
    }
    foo(10); // not ok
    foo(); 
}
foo(10);
foo(); // not ok - needs number