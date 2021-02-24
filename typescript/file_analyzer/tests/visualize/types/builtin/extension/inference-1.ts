interface Array<T> {
    equalsShallow<T>(this: ReadonlyArray<T>, other: ReadonlyArray<T>): boolean;
}

declare const a: (string | number)[] | null[] | undefined[] | {}[];
declare const b: (string | number)[] | null[] | undefined[] | {}[];

export let x = a.equalsShallow(b);
