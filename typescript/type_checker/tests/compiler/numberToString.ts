function f1(n:number):string {
    return n; // error return type mismatch
}

function f2(s:string):void {
}

f1(3);
f2(3); // error no coercion to string
f2(3+""); // ok + operator promotes
