// Repro from #29992

type NonOptionalKeys<T> = {
  [P in keyof T]: undefined extends T[P] ? never : P;
}[keyof T];
type Child<T> = { [P in NonOptionalKeys<T>]: T[P] };

export interface ListWidget {
  type: "list";
  minimum_count: number;
  maximum_count: number;
  collapsable?: boolean; //default to false, means all expanded
  each: Child<ListWidget>;
}

type ListChild = Child<ListWidget>;

declare let x: ListChild;
x.type;
