﻿// @declaration: true

type Foo<T, Y> = {
    foo<U, J>(): Foo<U, J>
};
export type SubFoo<R> = Foo<string, R>;

function foo() {
    return {} as SubFoo<number>;
}
