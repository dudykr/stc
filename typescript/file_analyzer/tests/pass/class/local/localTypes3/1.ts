function f<X>(x: X) {
    class C<Y> {
        public x = x;
        constructor(public y: Y) { }
    }
    return C;
}
let C = f(10);
let v = new C("hello");
v.x;
v.y;


export { }