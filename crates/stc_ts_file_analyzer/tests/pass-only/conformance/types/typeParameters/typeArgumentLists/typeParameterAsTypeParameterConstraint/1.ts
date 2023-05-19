// using a type parameter as a constraint for a type parameter is valid
// no errors expected except illegal constraints

function foo2<T, U extends { length: T }>(x: T, y: U) { return y; }
foo2(1, '');
foo2({}, { length: 2 });
foo2(1, { width: 3, length: 2 });
foo2(1, []);
foo2(1, ['']);

export { }