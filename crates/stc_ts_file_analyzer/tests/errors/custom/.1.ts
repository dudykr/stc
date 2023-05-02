// @strict: true
// @target: esnext

// Repro from #33490

declare class Component<P> {
  props: P;
}

export type ComponentClass<P> = new (props: P) => Component<P>;
export type FunctionComponent<P> = (props: P) => null;

export type ComponentType<P> = FunctionComponent<P> | ComponentClass<P>;

export interface RouteComponentProps {
  route: string;
}

declare function withRouter<
  P extends RouteComponentProps,
  C extends ComponentType<P>
>(
  component: C & ComponentType<P>
): ComponentClass<Omit<P, keyof RouteComponentProps>>;

interface Props extends RouteComponentProps {
  username: string;
}

declare const MyComponent: ComponentType<Props>;

withRouter(MyComponent);
