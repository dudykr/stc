// @strict: true

// Repro from #30050

interface Obj<T> {
	ref: T;
}
interface Func<T> {
	(x: T): void;
}
type UnionToIntersection<U> = (U extends any ? (k: U) => void : never) extends ((k: infer I) => void) ? I : never;
type CtorOf<T> = (arg: UnionToIntersection<T>) => T;

interface Big {
    "0": { common?: string; "0"?: number, ref?: Obj<Big["0"]> | Func<Big["0"]>; }
    "1": { common?: string; "1"?: number, ref?: Obj<Big["1"]> | Func<Big["1"]>; }
    "2": { common?: string; "2"?: number, ref?: Obj<Big["2"]> | Func<Big["2"]>; }
    "3": { common?: string; "3"?: number, ref?: Obj<Big["3"]> | Func<Big["3"]>; }
    "4": { common?: string; "4"?: number, ref?: Obj<Big["4"]> | Func<Big["4"]>; }
    "5": { common?: string; "5"?: number, ref?: Obj<Big["5"]> | Func<Big["5"]>; }
    "6": { common?: string; "6"?: number, ref?: Obj<Big["6"]> | Func<Big["6"]>; }
    "7": { common?: string; "7"?: number, ref?: Obj<Big["7"]> | Func<Big["7"]>; }
    "8": { common?: string; "8"?: number, ref?: Obj<Big["8"]> | Func<Big["8"]>; }
    "9": { common?: string; "9"?: number, ref?: Obj<Big["9"]> | Func<Big["9"]>; }
    "10": { common?: string; "10"?: number, ref?: Obj<Big["10"]> | Func<Big["10"]>; }
    "11": { common?: string; "11"?: number, ref?: Obj<Big["11"]> | Func<Big["11"]>; }
    "12": { common?: string; "12"?: number, ref?: Obj<Big["12"]> | Func<Big["12"]>; }
    "13": { common?: string; "13"?: number, ref?: Obj<Big["13"]> | Func<Big["13"]>; }
    "14": { common?: string; "14"?: number, ref?: Obj<Big["14"]> | Func<Big["14"]>; }
    "15": { common?: string; "15"?: number, ref?: Obj<Big["15"]> | Func<Big["15"]>; }
    "16": { common?: string; "16"?: number, ref?: Obj<Big["16"]> | Func<Big["16"]>; }
    "17": { common?: string; "17"?: number, ref?: Obj<Big["17"]> | Func<Big["17"]>; }
}
declare function getCtor<T extends keyof Big>(comp: T): CtorOf<Big[T]>

declare var all: keyof Big;
const ctor = getCtor(all);
const comp = ctor({ common: "ok", ref: x => console.log(x) });
