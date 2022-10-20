// @strict: true
export type ElChildren =
  | ElChildren.Void
  | ElChildren.Text;
export namespace ElChildren {
  export type Void = undefined;
  export type Text = string;
}

type Relax<C extends ElChildren> = C extends ElChildren.Text ? ElChildren.Text : C;

export class Elem<
  C extends ElChildren,
  > {
  constructor(
    private children_: Relax<C>,
  ) {
  }
}

new Elem(undefined as ElChildren.Void);
new Elem('' as ElChildren.Text);
new Elem('' as ElChildren.Void | ElChildren.Text); // error
new Elem('' as ElChildren); // error

// Repro from #31766

interface I { a: string }

type DeepPartial<T> =
    T extends object ? {[K in keyof T]?: DeepPartial<T[K]>} : T;

declare function f<T>(t: T, partial: DeepPartial<T>): T;

function g(p1: I, p2: Partial<I>): I {
  return f(p1, p2);
}
