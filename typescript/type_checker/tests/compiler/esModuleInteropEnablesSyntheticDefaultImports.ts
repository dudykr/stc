// @esModuleInterop: true
// @module: es2015
// @Filename: a.ts
import Namespace from "./b";
export var x = new Namespace.Foo();

// @Filename: b.d.ts
export class Foo {
	member: string;
}
