
export function f100<T, K extends keyof T>(obj: T, keys: K[]): void {
    for (const k of keys) {
        const item = obj[k];
        if (typeof item == 'function')
            item.call(obj);
    }
}
