function c<T>(data: string | T): T {
    if (typeof data === 'string') {
        return JSON.parse(data);
    } else {
        return data;
    }
}