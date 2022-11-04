declare class TableClass<S = any> {
    _field: S;
}

export type Table = TableClass;

interface Something {
    prop: number;
}

interface SomethingElse {
    prop2: string;
}

declare let aBoolean: boolean;
declare let aStringOrNumber: string | number;
declare let aStringOrSomething: string | Something;
declare let someUnion: Something | SomethingElse;

function fn<T extends Table>(o: T) {
    aBoolean = o;
    aStringOrNumber = o;
    aStringOrSomething = o;
    someUnion = o;
}

function fn2<T extends TableClass>(o: T) {
    aBoolean = o;
    aStringOrNumber = o;
    aStringOrSomething = o;
    someUnion = o;
}

declare const o: Table;
aBoolean = o;
aStringOrNumber = o;
aStringOrSomething = o;
someUnion = o;
