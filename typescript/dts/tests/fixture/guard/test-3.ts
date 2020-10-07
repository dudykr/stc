

export function isError(err: any): err is Error {
    return err instanceof Error;
}

export function test() {
    const err = {};


    if (isError(err)) {
        return err.message
    }
}