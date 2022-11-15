// Fix #19666

let symbol!: symbol;
let str = "hello ";

const templateStr = `hello ${symbol}`;
const appendStr = "hello " + symbol;
str += symbol;

let symbolUnionNumber!: symbol | number;
let symbolUnionString!: symbol | string;

const templateStrUnion = `union with number ${symbolUnionNumber} and union with string ${symbolUnionString}`;


// Fix #44462

type StringOrSymbol = string | symbol;

function getKey<S extends StringOrSymbol>(key: S) {
    return `${key} is the key`;
}

function getKey1<S extends symbol>(key: S) {
    let s1!: S;
    `${s1}`;
    s1 + '';
    +s1;

    let s2!: S | string;
    `${s2}`;
    s2 + '';
    +s2;
}

function getKey2<S extends string>(key: S) {
    let s1!: S;
    `${s1}`;
    s1 + '';
    +s1;

    let s2!: S | symbol;
    `${s2}`;
    s2 + '';
    +s2;
}
