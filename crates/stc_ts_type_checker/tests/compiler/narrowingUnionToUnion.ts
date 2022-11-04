// @strict: true
// @declaration: true

type Falsy = false | 0 | 0n | '' | null | undefined;

declare function isFalsy(value: unknown): value is Falsy;

function fx1(x: string | number | undefined) {
    if (isFalsy(x)) {
        x;  // "" | 0 | undefined
    }
}

function fx2<T>(x: T | undefined) {
    if (isFalsy(x)) {
        x;  // T & Falsy | undefined
    }
}

function fx3<T extends string | number>(x: T) {
    if (isFalsy(x)) {
        x;  // T & "" | T & 0
    }
}

declare function isA(obj: unknown): obj is { a: false } | { b: 0 };

function fx4(obj: { b: number }) {
    if (isA(obj)) {
        obj;  // { b: 0 }
    }
}

declare class X { x: string }
declare class XS extends X { xs: string }

declare class Y { y: string }
declare class YS extends Y { ys: string }

declare function isXSorY(obj: unknown): obj is XS | Y;

function fx5<T extends X>(obj: X | YS, c: typeof XS | typeof Y) {
    if (obj instanceof c) {
        obj;  // XS | YS
    }
    if (isXSorY(obj)) {
        obj;  // XS | YS
    }
}

// Repro from #31156

declare function isEmptyStrOrUndefined(mixed: any): mixed is "" | undefined;

function fx10(s: string | undefined) {
    if (isEmptyStrOrUndefined(s)) {
        s;  // "" | undefined
        if (s == undefined) {
            s;  // undefined
        }
        else {
            s;  // ""
        }
    }
}

// Repro from #37807

function f1(x: any): asserts x is number | undefined { }
let v1: number | string | undefined;
f1(v1);
v1;  // number | undefined

function f2(x: any): asserts x is 6 | undefined { }
let v2: number | string | undefined;
f2(v2);
v2;  // 6 | undefined

// #39105

declare function isEmptyString(value: string): value is '';
declare function isMaybeEmptyString(value: string | null | undefined): value is '' | null | undefined;

declare function isZero(value: number): value is 0;
declare function isMaybeZero(value: number | null | undefined): value is 0 | null | undefined;

declare function isEmptyArray<T>(value: T[]): value is [];
declare function isMaybeEmptyArray<T>(value: T[] | null | undefined): value is [] | null | undefined;

const TEST_CASES = [
    (value: string) => {
        if (isEmptyString(value)) {
            value;  // ""
        }
        else {
            value;  // string
        }
        if (isMaybeEmptyString(value)) {
            value;  // ""
        }
        else {
            value;  // string
        }
    },
    (value?: string) => {
        if (isMaybeEmptyString(value)) {
            value;  // "" | undefined
        }
        else {
            value;  // string
        }
    },
    (value: number) => {
        if (isZero(value)) {
            value;  // 0
        }
        else {
            value;  // number
        }
        if (isMaybeZero(value)) {
            value; // 0
        }
        else {
            value;  // number
        }
    },
    (value?: number) => {
        if (isMaybeZero(value)) {
            value;  // 0 | undefined
        }
        else {
            value;  // number
        }
    },
    (value: string[]) => {
        if (isEmptyArray(value)) {
            value;  // []
        }
        else {
            value;  // string[]
        }
        if (isMaybeEmptyArray(value)) {
            value;  // []
        }
        else {
            value;  // string[]
        }
    },
    (value?: string[]) => {
        if (isMaybeEmptyArray(value)) {
            value;  // [] | undefined
        }
        else {
            value;  // string[]
        }
    },
];

// Repro from #42101

type EmptyString = '' | null | undefined;

function isEmpty(value: string | EmptyString): value is EmptyString {
    return value === '' || value === null || value === undefined;
}

let test: string | null | undefined;

if (isEmpty(test)) {
    test;  // EmptyString
}

// Repro from #43825

declare function assert<T>(value: any): asserts value is T

function test1(foo: number | string | boolean) {
    assert<1 | string>(foo);
    foo;  // string | 1
}

// Repro from #46909

function check1(x: unknown): x is (string | 0) {
	return typeof x === "string" || x === 0;
}

function check2(x: unknown): x is ("hello" | 0) {
	return x === "hello" || x === 0;
}

function test3(x: unknown) {
	if (typeof x === "string" || x === 0) {
		x;  // string | 0
		if (x === "hello" || x === 0) {
			x;  // 0 | "hello"
		}
	}
	if (check1(x)) {
		x;  // string | 0
		if (check2(x)) {
			x;  // 0 | "hello"
		}
	}
}

// Repro from #49588

function assertRelationIsNullOrStringArray(v: (string | number)[] | null): asserts v is string[] | null {}

function f1x(obj: (string | number)[] | null) {
    assertRelationIsNullOrStringArray(obj);
    obj;  // string[] | null
}
