export function createErrorClass<T>(createImpl: (_super: any) => any): T {
    const _super = (instance: any) => {
        Error.call(instance);
        instance.name = instance.constructor.name;
        instance.stack = new Error().stack;
    };

    const ctorFunc = createImpl(_super);
    ctorFunc.prototype = Object.create(Error.prototype);
    ctorFunc.prototype.constructor = ctorFunc;
    return ctorFunc;
}
