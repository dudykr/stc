//@strict: true

declare function stringifyArray<T extends readonly any[]>(arr: T): { -readonly [K in keyof T]: string };
export let abc: any[] = stringifyArray(void 0 as any);
