// @strict: true
// @target: esnext

// Repros from #32247

export const g: <U, R, S>(com: () => Iterator<S, U, R> | AsyncIterator<S, U, R>) => Promise<U> = async <U, R, S>(com: () => Iterator<S, U, R> | AsyncIterator<S, U, R>): Promise<U> => {
  throw com;
};

