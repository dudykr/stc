// @target: esnext
//@strict: true

// #35995
export async function* f4(): AsyncGenerator<any, { x: 'x' }, any> {
  const ret = { x: 'x' } as const;
  return Promise.resolve(ret); // Error
}

