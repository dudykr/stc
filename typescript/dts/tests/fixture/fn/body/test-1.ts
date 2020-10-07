

export class Subscriber<T>{
    closed: boolean = false;
    next(value: T) { }
    complete() { }

    error(err: any) { }
}


export const subscribeToPromise = <T>(promise: PromiseLike<T>) => (subscriber: Subscriber<T>) => {
    promise.then(
        (value) => {
            if (!subscriber.closed) {
                subscriber.next(value);
                subscriber.complete();
            }
        },
        (err: any) => subscriber.error(err)
    )
        .then(null, (err) => console.log(err));
    return subscriber;
};