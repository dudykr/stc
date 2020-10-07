export const _super = (instance: any) => {
    Error.call(instance);
    instance.stack = new Error().stack;
};