// @target: es6
// @noImplicitAny: true

export interface StrategicState {
    lastStrategyApplied?: string;
}

export function strategy<T extends StrategicState>(stratName: string, gen: (a: T) => IterableIterator<T | undefined>): (a: T) => IterableIterator<T | undefined> {
    return function* (state) {
        for (const next of gen(state)) {
            if (next) {
                next.lastStrategyApplied = stratName;
            }
            yield next;
        }
    }
}
