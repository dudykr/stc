// @target: esnext
//@strict: true

export async function* f3(): AsyncGenerator<any, { x: 'x' }, any> {
  return Promise.resolve({ x: 'x' });
}

