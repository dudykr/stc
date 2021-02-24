

async function foo(): Promise<string> {
    return 'foo'
}


export async function bar() {
    await foo()
}