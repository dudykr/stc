// @strict: true
// @target: es6
// @declaration: true

// Repros from #5487

function truePromise(): Promise<true> {
    return Promise.resolve(true);
}

interface Wrap<T> {
    value: T;
}

function wrap<T>(value: T): Wrap<T> {
    return { value };
}

function wrappedFoo(): Wrap<'foo'> {
    return wrap('foo');
}

function wrapBar(value: 'bar'): Wrap<'bar'> {
    return { value };
}

function wrappedBar(): Wrap<'bar'> {
    const value = 'bar';
    const inferred = wrapBar(value);
    const literal = wrapBar('bar');
    const value2: string = 'bar';
    const literal2 = wrapBar(value2);  // Error
    return wrap(value);
}

function wrappedBaz(): Wrap<'baz'> {
    const value: 'baz' = 'baz';
    return wrap(value);
}

// Repro from #11152

interface FolderContentItem {
    type: 'folder' | 'file';
}

let a: FolderContentItem[] = [];
a = [1, 2, 3, 4, 5].map(v => ({ type: 'folder' }));

// Repro from #11312

let arr: Array<[number, number]> = [[1, 2]]

let mappedArr: Array<[number, number]> = arr.map(([x, y]) => {
    return [x, y];
})

// Repro from #13594

export namespace DiagnosticSeverity {
	export const Error = 1;
	export const Warning = 2;
	export const Information = 3;
	export const Hint = 4;
}

export type DiagnosticSeverity = 1 | 2 | 3 | 4;

export interface Diagnostic {
	severity?: DiagnosticSeverity;
	code?: number | string;
	source?: string;
	message: string;
}

function bug(): Diagnostic[] {
	let values: any[] = [];
	return values.map((value) => {
		return {
			severity: DiagnosticSeverity.Error,
			message: 'message'
		}
	});
}

// Repro from #22870

function objectToMap(obj: any) {
	return new Map(Object.keys(obj).map(key => [key, obj[key]]));
};

// Repro from #24352

interface Person {
  phoneNumbers: {
    __typename: 'PhoneNumber';
  }[];
}

function createPerson(): Person {
  return {
    phoneNumbers: [1].map(() => ({
      __typename: 'PhoneNumber'
    }))
  };
}

// Repro from #26621

type Box<T> = { value: T };
declare function box<T>(value: T): Box<T>;

type WinCondition =
    | { type: 'win', player: string }
    | { type: 'draw' };

let zz: Box<WinCondition> = box({ type: 'draw' });

type WinType = 'win' | 'draw';

let yy: Box<WinType> = box('draw');

// Repro from #27074

interface OK<T> {
    kind: "OK";
    value: T;
}
export function ok<T>(value: T): OK<T> {
    return {
        kind: "OK",
        value: value
    };
}

let result: OK<[string, number]> = ok(["hello", 12]);

// Repro from #25889

interface I {
    code: 'mapped',
    name: string,
}

const a3: I[] = ['a', 'b'].map(name => {
    return {
        code: 'mapped',
        name,
    }
});

// Repro from https://www.memsql.com/blog/porting-30k-lines-of-code-from-flow-to-typescript/
	
type Player = {
    name: string;
    age: number;
    position: "STRIKER" | "GOALKEEPER",
};
 
type F = () => Promise<Array<Player>>;
 
const f1: F = () => {
    return Promise.all([
        {
            name: "David Gomes",
            age: 23,
            position: "GOALKEEPER",
        }, {
            name: "Cristiano Ronaldo",
            age: 33,
            position: "STRIKER",
        }
    ]);
};

// Breaking change repros from #29478

declare function foldLeft<U>(z: U, f: (acc: U, t: boolean) => U): U;
let res: boolean = foldLeft(true, (acc, t) => acc && t);  // Error

enum State { A, B }
type Foo = { state: State }
declare function bar<T>(f: () => T[]): T[];
let x: Foo[] = bar(() => !!true ? [{ state: State.A }] : [{ state: State.B }]);  // Error

// Repros from #31443

enum Enum { A, B }

class ClassWithConvert<T> {
  constructor(val: T) { }
  convert(converter: { to: (v: T) => T; }) { }
}

function fn<T>(arg: ClassWithConvert<T>, f: () => ClassWithConvert<T>) { }
fn(new ClassWithConvert(Enum.A), () => new ClassWithConvert(Enum.A));

type Func<T> = (x: T) => T;

declare function makeFoo<T>(x: T): Func<T>;
declare function baz<U>(x: Func<U>, y: Func<U>): void;

baz(makeFoo(Enum.A), makeFoo(Enum.A));
