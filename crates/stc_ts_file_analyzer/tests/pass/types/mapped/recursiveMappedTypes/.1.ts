// @declaration: true


type NonOptionalKeys<T> = { [P in keyof T]: undefined extends T[P] ? never : P }[keyof T];
type Child<T> = { [P in NonOptionalKeys<T>]: T[P] }

export interface ListWidget {
    "type": "list",
    "minimum_count": number,
    "maximum_count": number,
    "collapsable"?: boolean, //default to false, means all expanded
    "each": Child<ListWidget>;
}
type A = NonOptionalKeys<ListWidget>; // expected any
// declare const a: A; // expected any
// a // expected any