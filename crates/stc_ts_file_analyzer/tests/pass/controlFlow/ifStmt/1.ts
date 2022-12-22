function d<T extends string>(data: string | T): never {
    if (typeof data === 'string') {
        throw new Error('will always happen');
    }
    else {
        return data;
    }
}