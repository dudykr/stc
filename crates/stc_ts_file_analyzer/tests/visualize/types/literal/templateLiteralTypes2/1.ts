//@strict: true
// Repro from #41631

export declare function takesLiteral<T extends string>(literal: T): T extends `foo.bar.${infer R}` ? R : unknown;

const t1 = takesLiteral("foo.bar.baz"); // "baz"
const id2 = "foo.bar.baz";
const t2 = takesLiteral(id2); // "baz"

declare const someString: string;
const t3 = takesLiteral(`foo.bar.${someString}`);  // string

const id4 = `foo.bar.${someString}`;
const t4 = takesLiteral(id4);  // unknown

declare const someUnion: 'abc' | 'def' | 'ghi';
const t5 = takesLiteral(`foo.bar.${someUnion}`);  // "abc" | "def" | "ghi"

