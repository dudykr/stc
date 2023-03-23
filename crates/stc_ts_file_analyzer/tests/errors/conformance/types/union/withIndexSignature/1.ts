//@strict: true

type RO = { foo: number } | { readonly [s: string]: string };
declare var ro: RO;
ro.foo = "not allowed";
