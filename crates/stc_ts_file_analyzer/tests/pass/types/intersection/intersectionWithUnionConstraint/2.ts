//@strict: true

function f2<
  T extends string | number | undefined,
  U extends string | null | undefined,
>(x: T & U) {
  x; // (T & U
  let _: string | undefined | number = x; // stirng | undefined
}
