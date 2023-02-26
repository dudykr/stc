interface StringTo<T> {
    [x: string]: T;
}

interface NumberTo<T> {
    [x: number]: T;
}

interface StringAndNumberTo<T> extends StringTo<T>, NumberTo<T> { }

interface Obj {
    hello: string;
    world: number;
}

type NumberToNumber = NumberTo<number>;

interface StringToAnyNumberToNumber extends StringTo<any>, NumberToNumber { }

function f3(
    sToAny: StringTo<any>,
    nToNumber: NumberToNumber,
    strToAnyNumToNum: StringToAnyNumberToNumber,
    someObj: Obj
) {
    someObj = sToAny;
}