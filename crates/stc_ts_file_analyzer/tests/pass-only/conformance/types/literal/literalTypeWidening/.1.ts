// Widening vs. non-widening literal types

// Repro from #23649

export function Set<K extends string>(...keys: K[]): Record<K, true | undefined> {
  const result = {} as Record<K, true | undefined>
  keys.forEach(key => result[key] = true)
  return result
}

export function keys<K extends string, V>(obj: Record<K, V>): K[] {
  return Object.keys(obj) as K[]
}

type Obj = { code: LangCode }

const langCodeSet = Set('fr', 'en', 'es', 'it', 'nl')
export type LangCode = keyof typeof langCodeSet
export const langCodes = keys(langCodeSet)

const arr: Obj[] = langCodes.map(code => ({ code }))

// Repro from #29081

function test<T extends { a: string, b: string }>(obj: T): T {
  let { a, ...rest } = obj;
  return { a: 'hello', ...rest } as T;
}

// Repro from #32169

declare function f<T>(x: T): NonNullable<T>;
enum E { A, B }
const a = f(E.A);
const b: E.A = a;
