export declare const config: any;

export function reportUnhandledError(err: any) {
    setTimeout(() => {
        const { onUnhandledError } = config;
        if (onUnhandledError) {
            // Execute the user-configured error handler.
            onUnhandledError(err);
        } else {
            // Throw so it is picked up by the runtime's uncaught error mechanism.
            throw err;
        }
    });
}

