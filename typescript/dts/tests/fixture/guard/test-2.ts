export function isError(err: any): err is Error {
    return err instanceof Error;
}

export function getMsg(err: any): string {
    if (isError(err)) {
        return err.message
    }

    return ''
}


export const res1 = isError('foo')
export const msg2 = getMsg('foo')