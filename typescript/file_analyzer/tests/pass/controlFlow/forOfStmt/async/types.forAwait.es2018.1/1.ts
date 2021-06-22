declare const iterableOfPromise: Iterable<Promise<number>>;
async function f1() {
    let y: number;
    for await (y of iterableOfPromise) {
    }
}

export { }