
export interface TimeoutInfo<T, M = unknown> {
    readonly meta: M;
    readonly seen: number;
    readonly lastValue: T | null;
}

export function make(info: TimeoutInfo<any> | null = null) {
    return info
}

export declare const info: TimeoutInfo<any, string>
export const res1 = make(info);



