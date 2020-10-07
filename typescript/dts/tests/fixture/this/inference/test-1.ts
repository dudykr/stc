export interface SchedulerLike {
    schedule(work: (this: Array<string>) => void): Array<string>;
}

export declare const scheduler: SchedulerLike;

export const a = scheduler.schedule(function () {
    return this
});