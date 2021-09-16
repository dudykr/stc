declare var a: new (a: string) => string;
declare var b: {
    new(a: number): number;
    new(a: string): string;
};

a = b;

export { }