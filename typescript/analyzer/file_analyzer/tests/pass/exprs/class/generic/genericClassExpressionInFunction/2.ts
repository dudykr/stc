class A<T> {
    genericVar: T
}
function B1<U>() {
    // class expression can use T
    return class extends A<U> { }
}
// extends can call B
class K extends B1<number>() {
    namae: string;
}
var k = new K();
k.genericVar = 12;


export { }