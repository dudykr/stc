

export function isError(err: any): err is Error {
    return err instanceof Error;
}