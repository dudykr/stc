

export const gen = function* () {
    yield* {
        *[Symbol.iterator]() {
            yield (x: string) => x.length
        }
    }
};