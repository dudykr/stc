class Base {
    a?: number;

    f?(): number;
}

class Derived extends Base {
    a = 1;

    f(): number {
        return 1;
    }
}
