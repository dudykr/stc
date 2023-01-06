//@strict: true
// Repro from #41631

export declare function takesLiteral<T extends string>(literal: T): T extends `foo.bar.${infer R}` ? R : unknown;

declare const someUnion: 'abc' | 'def' | 'ghi';
export const t5 = takesLiteral(`foo.bar.${someUnion}`);  // "abc" | "def" | "ghi"

