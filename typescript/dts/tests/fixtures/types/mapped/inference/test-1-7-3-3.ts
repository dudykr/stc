declare type Box<BoxT> = {
    value: BoxT;
};
declare type Boxified<BoxifiedT> = {
    [BoxifiedP in keyof BoxifiedT]: Box<BoxifiedT[BoxifiedP]>;
};

declare function f22<T, K extends keyof T>(obj: Boxified<Pick<T, K>>): T;

let x2 = f22({ foo: { value: 42 }, bar: { value: "hello" } });
