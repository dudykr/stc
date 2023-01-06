//@strict: true
// Repro from #41631

export declare function takesLiteral<T extends string>(literal: T): T extends `foo.bar.${infer R}` ? R : unknown;

declare const someString: string;

const id4 = `foo.bar.${someString}`;
export const t4 = takesLiteral(id4);  // unknown


