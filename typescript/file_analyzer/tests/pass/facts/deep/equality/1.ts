

interface A {
    type: 'a'
}

interface B {
    type: 'b'
}

export declare const foo: { obj: { obj2: A | B } };


if (foo.obj.obj2.type === 'a') {
    const a: 'a' = foo.obj.obj2.type;
}

