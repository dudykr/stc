

type T1 = (((x: string) => string) & ((x: number) => number));



declare let a: T1;

a('s')
a(1)

export { }