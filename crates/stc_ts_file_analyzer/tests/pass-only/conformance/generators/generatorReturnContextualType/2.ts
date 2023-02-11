// @target: esnext
//@strict: true

export async function* g3(): AsyncIterator<any, { x: 'x' }, any> {
  return Promise.resolve({ x: 'x' });
}
