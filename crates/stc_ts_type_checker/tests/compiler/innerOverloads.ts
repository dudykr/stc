
function outer() {
    function inner(x:number); // should work
    function inner(x:string);
    function inner(a:any) { return a; }

    return inner(0);
}

var x = outer(); // should work

