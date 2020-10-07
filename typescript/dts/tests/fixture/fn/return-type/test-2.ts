const config = {
    onUnhandledError: null as ((err: any) => void) | null,

    Promise: undefined! as PromiseConstructorLike,

    useDeprecatedSynchronousErrorHandling: false,

    useDeprecatedNextContext: false,
};


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

