export class Subscriber<T>  {
    add(arg: any) {

    }
}
export class SafeSubscriber<T> extends Subscriber<T> {
    constructor(...args: any[]) {
        super()
    }
}

export declare function isSubscriber<T>(value: any): value is Subscriber<T>;


export class Observable<T>{
    source: any;

    subscribe(
        observerOrNext?: ((value: T) => void) | null,
        error?: ((error: any) => void) | null,
        complete?: (() => void) | null
    ) {
        const subscriber = isSubscriber(observerOrNext) ? observerOrNext : new SafeSubscriber(observerOrNext, error, complete);

        return subscriber;
    }

}