//@strict: true
// Repro from #41631

export declare function takesLiteral<T extends string>(literal: T): T extends `foo.bar.${infer R}` ? R : unknown;

declare const someString: string;
export const t3 = takesLiteral(`foo.bar.${someString}`);  // string


