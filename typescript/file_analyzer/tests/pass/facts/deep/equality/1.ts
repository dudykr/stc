

interface A {
    type: 'a'
}

interface B {
    type: 'b'
}

export declare const foo: { obj: { obj2: A | B } };


function useA(a: A) {
}

if (foo.obj.obj2.type === 'a') {
    useA(foo.obj.obj2)
}

