// call signatures in derived types must have the same or fewer optional parameters as the target for assignment

module ClassTypeParam {
    class Base<T> {
        a: () => T;

        init = () => {
            this.a = (x: T) => null; // error, too many required params
        }
    }
}

