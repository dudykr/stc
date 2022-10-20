export function arrRemove<T>(arr: T[] | undefined | null, item: T) {
    if (arr) {
        const index = arr.indexOf(item);
        0 <= index && arr.splice(index, 1);
    }
}
