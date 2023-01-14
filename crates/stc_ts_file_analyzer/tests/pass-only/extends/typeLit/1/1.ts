//@strict: true


type A = {
    f(): void;
}

type B = {
    f(x?: string): void;
    g(): void;
}


type T2 = Extract<A, B>

export let _21: T2 = null as any as never;
export let _22: B = null as any as T2;