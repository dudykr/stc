var obj17: any;
if (obj17 instanceof Object) {
  obj17; // any
  // can't narrow type from 'any' to 'Object'
  obj17.foo1;
  obj17.foo2;
}

var obj18: any;
if (obj18 instanceof Function) {
  obj18; // any;
  // can't narrow type from 'any' to 'Function'
  obj18.foo1;
  obj18.foo2;
}
