// a construct signature with generics
interface BConstructor {
  new <T>(): B<T>;
}
interface B<T> {
  foo: T;
}
declare var B: BConstructor;

var obj4: any;
if (obj4 instanceof B) {
  obj4; // B<any>
  obj4.foo = "str";
  obj4.foo = 1;
}
