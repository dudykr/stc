// @target: es2015

// Repros from #29815

interface ITest {
    name: 'test'
}

export const createTestAsync = (): Promise<ITest> => Promise.resolve().then(() => ({ name: 'test' }))
