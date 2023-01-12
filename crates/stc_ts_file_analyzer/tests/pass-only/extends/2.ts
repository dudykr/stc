//@strict: true


type A = {
    f(): void;
}

type B = {
    f(x?: string): void;
    g(): void;
}


type T1 = Extract<A | B, A>

export let _11: T1 = null as any as A | B;
export let _12: A | B = null as any as T1;