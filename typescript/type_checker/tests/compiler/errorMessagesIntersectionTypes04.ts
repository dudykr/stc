interface A {
    a;
}

interface B {
    b;
}

function f<T, U extends A, V extends U>(): void {
    let num: number;
    let bool: boolean;
    let str: string;

    let a_and_b: A & B;
    let num_and_bool: number & boolean;

    num = a_and_b;
    bool = a_and_b;
    str = a_and_b;

    str = num_and_bool;
}