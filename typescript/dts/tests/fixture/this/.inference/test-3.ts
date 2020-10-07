export interface SchedulerLike {
    schedule<T>(work: (this: Array<string>) => T): T;
}

export declare const scheduler: SchedulerLike;

export const a = scheduler.schedule(function () {
    return this.map((v) => v)
});