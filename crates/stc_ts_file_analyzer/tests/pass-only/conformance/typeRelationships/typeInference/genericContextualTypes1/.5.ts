// @strict: true
// @declaration: true

type Box<A> = { value: A };

const arrayFilter = <B>(f: (x: B) => boolean) => (a: B[]) => a.filter(f);

export const f31: <C extends Box<number>>(a: C[]) => C[] = arrayFilter(x => x.value > 10);
