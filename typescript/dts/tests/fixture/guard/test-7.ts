
export interface NextObserver<T> {
    closed?: boolean;
    next: (value: T) => void;
    error?: (err: any) => void;
    complete?: () => void;
}

export interface ErrorObserver<T> {
    closed?: boolean;
    next?: (value: T) => void;
    error: (err: any) => void;
    complete?: () => void;
}

export interface CompletionObserver<T> {
    closed?: boolean;
    next?: (value: T) => void;
    error?: (err: any) => void;
    complete: () => void;
}

export type PartialObserver<T> = NextObserver<T> | ErrorObserver<T> | CompletionObserver<T>;

export interface Observer<T> {
    closed?: boolean;
    next: (value: T) => void;
    error: (err: any) => void;
    complete: () => void;
}


export class Subscriber<T>  {
    add(arg: any) {

    }
}
export class SafeSubscriber<T> extends Subscriber<T> {
    constructor(...args: any[]) {
        super()
    }
}

function isObserver<T>(value: any): value is Observer<T> {
    return value && typeof value.next === 'function' && typeof value.error === 'function' && typeof value.complete === 'function';
}

function isSubscriber<T>(value: any): value is Subscriber<T> {
    return (value && value instanceof Subscriber) || (isObserver(value) && isSubscription(value));
}

export class Subscription { }

export function isSubscription(value: any): value is Subscription {
    return (
        value instanceof Subscription ||
        (value &&
            'closed' in value &&
            typeof value.remove === 'function' &&
            typeof value.add === 'function' &&
            typeof value.unsubscribe === 'function')
    );
}


export class Observable<T>{
    operator: any;
    source: any;


    subscribe(
        observerOrNext?: PartialObserver<T> | ((value: T) => void) | null,
        error?: ((error: any) => void) | null,
        complete?: (() => void) | null
    ) {
        const subscriber = isSubscriber(observerOrNext) ? observerOrNext : new SafeSubscriber(observerOrNext, error, complete);

        const { operator, source } = this;
        subscriber.add(operator
            ? operator.call(subscriber, source)
            : source
                ? () => { }
                : () => { }
        );

        return subscriber;
    }

}