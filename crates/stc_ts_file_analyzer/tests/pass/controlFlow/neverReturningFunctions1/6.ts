//@strict: true
//@allowUnreachableCode: false

export interface Component<T extends object = any> {
  attrName?: string;
  data: T;
  dependencies?: string[];
  el: any;
  id: string;
  multiple?: boolean;
  name: string;
  schema: unknown;
  system: any;

  init(data?: T): void;
  pause(): void;
  play(): void;
  remove(): void;
  tick?(time: number, timeDelta: number): void;
  update(oldData: T): void;
  updateSchema?(): void;

  extendSchema(update: unknown): void;
  flushToDOM(): void;
}

export interface ComponentConstructor<T extends object> {
  new (el: unknown, attrValue: string, id: string): T & Component;
  prototype: T & {
    name: string;
    system: unknown;
    play(): void;
    pause(): void;
  };
}

declare function registerComponent<T extends object>(
  name: string,
  component: ComponentDefinition<T>,
): ComponentConstructor<T>;

export type ComponentDefinition<T extends object = object> = T &
  Partial<Component> &
  ThisType<T & Component>;
export type Some = ComponentDefinition<object>;
const Component = registerComponent("test-component", {
  schema: {
    myProperty: {
      default: [],
      parse() {
        return [true];
      },
    },
    string: { type: "string" },
    num: 0,
  },
  init() {
    this.data.num = 0;
    this.el.setAttribute("custom-attribute", "custom-value");
  },
  update() {},
  tick() {},
  remove() {},
  pause() {},
  play() {},

  multiply(f: number) {
    // Reference to system because both were registered with the same name.
    return f * this.data.num * this.system!.data.counter;
  },
});
