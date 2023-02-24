type Foo = { foo: string };
type Bar = { bar: string };

declare function fooBar(x: { foo: string; bar: string }): void;
declare function fooBat(x: { foo: string; bat: string }): number;
type Extract2<T, U, V> = T extends U ? (T extends V ? T : never) : never;

function f20<T>(
  x: Extract<Extract<T, Foo>, Bar>,
  y: Extract<T, Foo & Bar>,
  z: Extract2<T, Foo, Bar>
) {
  // fooBar(x);
  // fooBar(y);
  // fooBar(z);
  const result = fooBat(x);
  // fooBat(y);
  // fooBat(z);
}
