declare function test<T>(a: T): [T]

export const res = test('foo') // [string]

