function f2() {
    function f<X>(x: X) {
        class C<Y> {
            public x = x;
            constructor(public y: Y) { }
        }
        return C;
    }
    let C = f(10);
    let v = new C("hello");
    let x = v.x;
    let y = v.y;
}
export { }