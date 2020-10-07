export class Observable<T> {
    lift: any
}

export interface UnaryFunction<T, R> { (source: T): R; }

export interface OperatorFunction<T, R> extends UnaryFunction<Observable<T>, Observable<R>> { }

export function hasLift(source: any): source is { lift: InstanceType<typeof Observable>['lift'] } {
    return typeof source?.lift === 'function';
}

export function operate(v: any) {
    if (hasLift(v)) {
        return v
    }
    throw new Error('')
}
