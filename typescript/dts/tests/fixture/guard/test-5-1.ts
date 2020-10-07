
export class Subscriber { }

export class DummySubscriber extends Subscriber { }

export class SafeSubscriber extends Subscriber { }

export declare function isSubscriber(val: any): val is DummySubscriber;

export declare const observerOrNext: (() => void) | Subscriber;

export const subscriber = isSubscriber(observerOrNext) ? observerOrNext : new SafeSubscriber();

