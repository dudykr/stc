export const test = <T>(iterable: Iterable<T>) => (subscriber: Promise<T>) => {
    if (Math.random() > 0.5) {
        return
    }


}