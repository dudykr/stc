
var a18: {
    new(x: {
        new(a: number): number;
        new(a: string): string;
    }): any[];
    new(x: {
        new(a: boolean): boolean;
        new(a: Date): Date;
    }): any[];
}

var b18: new <T>(x: new (a: T) => T) => T[];
a18 = b18; // ok
b18 = a18; // ok

export { }