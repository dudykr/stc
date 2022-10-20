interface Promise<T> {
    then<U>(success?: (value: T) => U): Promise<U>;
    then<U>(success?: (value: T) => Promise<U>): Promise<U>;
    value: T;
}
