interface IBar {
    [barId: string]: any;
}

interface IFoo {
    foo<TBar extends IBar>(bar: TBar, bar1: (bar: TBar) => TBar, bar2: (bar: TBar) => TBar): TBar;
}

var foo: IFoo;
foo.foo({ bar: null }, bar => null, bar => null);