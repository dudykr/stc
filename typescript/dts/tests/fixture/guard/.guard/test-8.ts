export class Observable<T> {
    lift: any
}

export interface UnaryFunction<T, R> { (source: T): R; }

export interface OperatorFunction<T, R> extends UnaryFunction<Observable<T>, Observable<R>> { }

export function hasLift(source: any): source is { lift: InstanceType<typeof Observable>['lift'] } {
    return typeof source?.lift === 'function';
}

export function operate<T, R>(
    init: (liftedSource: Observable<T>) => (() => void) | void
): OperatorFunction<T, R> {
    return (source: Observable<T>) => {
        if (hasLift(source)) {
            return source.lift(() => init);
        }
        throw new TypeError('Unable to lift unknown Observable type');
    };
}
