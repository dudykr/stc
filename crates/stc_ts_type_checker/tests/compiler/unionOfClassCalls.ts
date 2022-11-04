// @strict: true
// from https://github.com/microsoft/TypeScript/issues/30717
declare class Test<T> {
    obj: T;
    get<K extends keyof T>(k: K): T[K];
}

interface A { t: "A" }
interface B { t: "B" }

declare const tmp: Test<A> | Test<B>;

switch (tmp.get('t')) {
    case 'A': break;
    case 'B': break;
}

// from https://github.com/microsoft/TypeScript/issues/36390

const arr: number[] | string[] = [];  // Works with Array<number | string>
const arr1: number[]  = [];
const arr2:  string[] = [];

arr.map((a: number | string, index: number) => { 
    return index
})

// This case still doesn't work because `reduce` has multiple overloads :(
arr.reduce((acc: Array<string>, a: number | string, index: number) => { 
    return []
}, [])

arr.forEach((a: number | string, index: number) => { 
    return index
})

arr1.map((a: number, index: number) => { 
    return index
})

arr1.reduce((acc: number[], a: number, index: number) => { 
    return [a]
}, [])

arr1.forEach((a: number, index: number) => { 
    return index
})
arr2.map((a:  string, index: number) => { 
    return index
})

arr2.reduce((acc: string[], a: string, index: number) => { 
    return []
}, [])

arr2.forEach((a: string, index: number) => { 
    return index
})

// from https://github.com/microsoft/TypeScript/issues/36307

declare class Foo {
    doThing(): Promise<this>
}

declare class Bar extends Foo {
    bar: number;
}
declare class Baz extends Foo {
    baz: number;
}

declare var a: Bar | Baz;
// note, you must annotate `result` for now
a.doThing().then((result: Bar | Baz) => {
	// whatever
});
