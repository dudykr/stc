class A<T> {
    genericVar: T
}
function B3<W>() {
    return class Inner<TInner> extends A<W> { }
}
// extends can call B
let b3Number = B3<number>();
class S extends b3Number<string> {
    nom: string;
}
var s = new S();
s.genericVar = 12;

export { }