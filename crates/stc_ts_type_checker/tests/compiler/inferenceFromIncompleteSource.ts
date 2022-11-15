// @strict: true

// Repro from #42030

interface ListProps<T, K extends keyof T> {
  items: T[];
  itemKey: K;
  prop: number;
}

declare const Component: <T, K extends keyof T>(x: ListProps<T, K>) => void;

Component({items: [{name:' string'}], itemKey: 'name' });
