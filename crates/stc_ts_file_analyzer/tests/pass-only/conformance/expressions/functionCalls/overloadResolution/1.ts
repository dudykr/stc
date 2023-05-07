// Generic overloads with constraints called with type arguments that satisfy the constraints
function fn4<T extends string, U extends number>(n: T, m: U);
function fn4<T extends number, U extends string>(n: T, m: U);
function fn4() { }
fn4<string, number>('', 3);
fn4<number, string>(3, '');

// Generic overloads with constraints called without type arguments but with types that satisfy the constraints
fn4('', 3);
fn4(3, '');

export { }
