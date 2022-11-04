// @target: ES5

class A {
  get x() { return 0; }
  protected set x(v: number) { }

  public get y() { return 0; }
  protected set y(v: number) { }
}

class B {
  get q() { return 0; }
  protected set q(v: number) { }

  protected get u() { return 0; }
  protected set u(v: number) { }

  foo(this: A, a: A, b: B) {
    // Should have no errors in this block
    this.x = 0;
    this.y = 0;
    a.x = 0;
    a.y = 0;
    b.q = 0;
    b.u = 0;
  }
}

function bar(this: A, a: A, b: B) {
    this.x = 0;
    this.y = 0;
    a.x = 0;
    a.y = 0;
    // These should error
    b.q = 0;
    b.u = 0;
}
