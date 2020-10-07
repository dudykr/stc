export let resolved: Promise<any>;

/**
 * Helper functions to schedule and unschedule microtasks.
 */
export const Immediate = {
    setImmediate(cb: () => void) {
        if (!resolved) {
            resolved = Promise.resolve();
        }
    },
};