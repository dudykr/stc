// @declaration: true


type NonOptionalKeys<T> = { [P in keyof T]: undefined extends T[P] ? never : P }[keyof T];
type Child<T> = { [P in NonOptionalKeys<T>]: T[P] }

export interface ListWidget {
    "type": "list",
    "minimum_count": number,
    "maximum_count": number,
    "collapsable"?: boolean, //default to false, means all expanded
}
type A = Child<ListWidget>; // { type: "list"; minimum_count: number; maximum_count: number; }
declare const a: A; // { type: "list"; minimum_count: number; maximum_count: number; }
a // { type: "list"; minimum_count: number; maximum_count: number; }