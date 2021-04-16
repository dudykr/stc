export type Weird1 = (<U extends boolean>(a: U) => never) extends
    (<U extends true>(a: U) => never) ? never : never;

export type Weird2 = (<U extends boolean>(a: U) => U) extends
    (<U extends true>(a: U) => infer T) ? T : never;