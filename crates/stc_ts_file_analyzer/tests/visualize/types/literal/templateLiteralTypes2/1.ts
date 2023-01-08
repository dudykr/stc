//@strict: true
// Repro from #41631

export declare function takesLiteral<T extends string>(literal: T): T extends `foo.bar.${infer R}` ? R : unknown;

export const t1 = takesLiteral("foo.bar.baz"); // "baz"
