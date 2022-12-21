// @declaration: true


type NonOptionalKeys<T> = { [P in keyof T]: undefined extends T[P] ? never : P }[keyof T];
type Child<T> = { [P in NonOptionalKeys<T>]: T[P] }

export interface ListWidget {
    "type": "list",
    "minimum_count": number,
    "maximum_count": number,
    "collapsable"?: boolean, //default to false, means all expanded
}

type A = NonOptionalKeys<ListWidget>; // "type" | "minimum_count" | "maximum_count" | undefined
declare const a: A; // "type" | "minimum_count" | "maximum_count" | undefined
a // "type" | "minimum_count" | "maximum_count" | undefined

