class C {
    x1(a: number, callback: (x: 'hi') => number); // error
    x1(a: number, callback: (x: any) => number) {
        callback('hi');
        callback('bye');
        var hm = "hm";
        callback(hm);
    }
}