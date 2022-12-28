
export enum E {
    A, B
}

export declare let a: { [s: string]: string | E.A | E.B }
export declare function call(t: { [s: string]: string | E.A | E.B });

a = E
call(E)