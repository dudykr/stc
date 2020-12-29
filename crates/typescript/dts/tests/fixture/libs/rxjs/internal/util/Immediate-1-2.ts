export let nextHandle = 1;
// The promise needs to be created lazily otherwise it won't be patched by Zones
export let resolved: Promise<any>;
export const activeHandles: { [key: number]: any } = {};

/**
 * Finds the handle in the list of active handles, and removes it.
 * Returns `true` if found, `false` otherwise. Used both to clear
 * Immediate scheduled tasks, and to identify if a task should be scheduled.
 */
export function findAndClearHandle(handle: number): boolean {
    if (handle in activeHandles) {
        delete activeHandles[handle];
        return true;
    }
    return false;
}

/**
 * Helper functions to schedule and unschedule microtasks.
 */
export const Immediate = {
    clearImmediate(handle: number): void {
        findAndClearHandle(handle);
    },
};