function x2(a: number, cb: (x: 'hi') => number);
function x2(a: number, cb: (x: 'bye') => number);
function x2(a: number, cb: (x: any) => number) {
    cb('hi');
    cb('bye');
    var hm = 'hm';
    cb(hm); // should this work without a string definition?
    cb('uh');
    cb(1); 
}

var cb: (number) => number = (x: number) => 1;
x2(1, cb); // error
x2(1, (x: 'hi') => 1); // error
x2(1, (x: string) => 1);