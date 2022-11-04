// @target: es2015
// @declaration: true
// GH#37454, GH#41044

type O = { a?: string; b: number; c: number; };
type F1 = (arg: number) => any; // OK
type F2 = ({ a: string }: O) => any; // Error
type F3 = ({ a: string, b, c }: O) => any; // Error
type F4 = ({ a: string }: O) => any; // Error
type F5 = ({ a: string, b, c }: O) => any; // Error
type F6 = ({ a: string }) => typeof string; // OK
type F7 = ({ a: string, b: number }) => typeof number; // Error
type F8 = ({ a, b: number }) => typeof number; // OK
type F9 = ([a, b, c]) => void; // OK

type G1 = new (arg: number) => any; // OK
type G2 = new ({ a: string }: O) => any; // Error
type G3 = new ({ a: string, b, c }: O) => any; // Error
type G4 = new ({ a: string }: O) => any; // Error
type G5 = new ({ a: string, b, c }: O) => any; // Error
type G6 = new ({ a: string }) => typeof string; // OK
type G7 = new ({ a: string, b: number }) => typeof number; // Error
type G8 = new ({ a, b: number }) => typeof number; // OK
type G9 = new ([a, b, c]) => void; // OK

// Below are Error but renaming is retained in declaration emit,
// since elinding it would leave invalid syntax.
type F10 = ({ "a": string }) => void; // Error
type F11 = ({ 2: string }) => void; // Error
type F12 = ({ ["a"]: string }: O) => void; // Error
type F13 = ({ [2]: string }) => void; // Error
type G10 = new ({ "a": string }) => void; // Error
type G11 = new ({ 2: string }) => void; // Error
type G12 = new ({ ["a"]: string }: O) => void; // Error
type G13 = new ({ [2]: string }) => void; // Error

interface I {
  method1(arg: number): any; // OK
  method2({ a: string }): any; // Error

  (arg: number): any; // OK
  ({ a: string }): any; // Error

  new (arg: number): any; // OK
  new ({ a: string }): any; // Error
}

// Below are OK but renaming should be removed from declaration emit
function f1({ a: string }: O) { }
const f2 = function({ a: string }: O) { };
const f3 = ({ a: string, b, c }: O) => { };
const f4 = function({ a: string }: O): typeof string { return string; };
const f5 = ({ a: string, b, c }: O): typeof string => '';
const obj1 = {
  method({ a: string }: O) { }
};
const obj2 = {
  method({ a: string }: O): typeof string { return string; }
};
function f6({ a: string = "" }: O) { }
const f7 = ({ a: string = "", b, c }: O) => { };
const f8 = ({ "a": string }: O) => { };
function f9 ({ 2: string }) { };
function f10 ({ ["a"]: string }: O) { };
const f11 =  ({ [2]: string }) => { };

// In below case `string` should be kept because it is used
function f12({ a: string = "" }: O): typeof string { return "a"; }