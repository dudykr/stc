// with object type literal
interface D {
  foo: string;
}
declare var D: { new (): D };

var obj8: any;
if (obj8 instanceof D) {
  obj8; // D
  obj8.foo;
}
