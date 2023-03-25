// @strict: true
// @declaration: true

type Foo = { [key: string]: unknown };
type NullableFoo = Foo | undefined;

export type Bar<T extends NullableFoo> = NonNullable<T>[string];
