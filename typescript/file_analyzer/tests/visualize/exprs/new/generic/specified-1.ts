class D<T>{
    source: T;
    recurse: D<T>;
    wrapped: D<D<T>>
}

export var aGenericClass: D<string> = new D<string>();