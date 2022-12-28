
export enum E {
    A, B
}

export declare let a: { [s: string]: string | E }
export declare function call(t: { [s: string]: string | E });

a = E
call(E)