// @target: es5

class One {
  get prop1(): string { return ""; }
  set prop1(s: string | number) { }

  prop2: number;
}

class Two {
  get prop1(): "hello" { return "hello"; }
  set prop1(s: "hello" | number) { }

  get prop2(): string { return ""; }
  set prop2(s: string | 42) { }

}

declare const i: One & Two;

// "hello"
i.prop1;
// number | "hello"
i.prop1 = 42;
i.prop1 = "hello";

// never
i.prop2;
// 42
i.prop2 = 42;
i.prop2 = "hello"; // error
