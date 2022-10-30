// @strict: true
// @declaration: true

export const arrayFilter = <T>(f: (x: T) => boolean) => (a: T[]) => a.filter(f);

