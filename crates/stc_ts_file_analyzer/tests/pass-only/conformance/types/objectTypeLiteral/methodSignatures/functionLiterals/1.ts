var c2: {
    func4<T>(x: T): number;
    func4<T>(s: T): string;
    func5: {
        <T>(x: T): number;
        <T>(s: T): string;
    };
};

// no errors
c2.func4 = c2.func5;
c2.func5 = c2.func4;

export { }