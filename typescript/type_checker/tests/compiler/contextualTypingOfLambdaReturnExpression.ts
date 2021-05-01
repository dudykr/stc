function callb(lam: (l: number) => void);
function callb(lam: (n: string) => void);
function callb(a) { }

callb((a) => a.length); // Ok, we choose the second overload because the first one gave us an error when trying to resolve the lambda return type
callb((a) => { a.length; }); // Error, we picked the first overload and errored when type checking the lambda body