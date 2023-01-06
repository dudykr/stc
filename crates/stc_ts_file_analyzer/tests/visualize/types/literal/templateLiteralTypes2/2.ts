//@strict: true
// Repro from #41631

export declare function takesLiteral<T extends string>(literal: T): T extends `foo.bar.${infer R}` ? R : unknown;

const id2 = "foo.bar.baz";
export const t2 = takesLiteral(id2); // "baz"