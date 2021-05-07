// module then var
module m1 { }
var m1 = 1; // Should be allowed

module m1a { var y = 2; } // error
var m1a = 1; // error

module m1b { export var y = 2; } // error
var m1b = 1; // error

module m1c {
    export interface I { foo(): void; }
}
var m1c = 1; // Should be allowed

module m1d { // error
    export class I { foo() { } }
}
var m1d = 1; // error

// module then function
module m2 { }
function m2() { }; // ok since the module is not instantiated

module m2a { var y = 2; }
function m2a() { }; // error since the module is instantiated

module m2b { export var y = 2; }
function m2b() { };  // error since the module is instantiated

// should be errors to have function first
function m2c() { }; 
module m2c { export var y = 2; } 

module m2d { }
declare function m2d(): void; 

declare function m2e(): void; 
module m2e { }

function m2f() { };
module m2f { export interface I { foo(): void } } 

function m2g() { };
module m2g { export class C { foo() { } } } 

// module then class
module m3 { }
class m3 { } // ok since the module is not instantiated

module m3a { var y = 2; }
class m3a { foo() { } } // error, class isn't ambient or declared before the module

class m3b { foo() { } }
module m3b { var y = 2; }

class m3c { foo() { } }
module m3c { export var y = 2; } 

declare class m3d { foo(): void }
module m3d { export var y = 2; } 

module m3e { export var y = 2; } 
declare class m3e { foo(): void } 

declare class m3f { foo(): void }
module m3f { export interface I { foo(): void } }

declare class m3g { foo(): void }
module m3g { export class C { foo() { } } }

// module then enum
// should be errors
module m4 { }
enum m4 { }

module m4a { var y = 2; }
enum m4a { One }

module m4b { export var y = 2; }
enum m4b { One }

module m4c { interface I { foo(): void } }
enum m4c { One }

module m4d { class C { foo() { } } }
enum m4d { One }

//// module then module

module m5 { export var y = 2; }
module m5 { export interface I { foo(): void } } // should already be reasonably well covered

// module then import
module m6 { export var y = 2; }
//import m6 = require('');
