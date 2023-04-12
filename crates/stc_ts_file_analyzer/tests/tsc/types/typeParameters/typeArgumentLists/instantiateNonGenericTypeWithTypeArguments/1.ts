// it is an error to provide type arguments to a non-generic call
// all of these are errors


var f: { (): void };
export var r2 = new f<number>();
