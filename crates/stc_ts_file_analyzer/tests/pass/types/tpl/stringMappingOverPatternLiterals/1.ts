// non-template
type A = "aA";
type B = Uppercase<A>;
type C = Lowercase<A>;

// templated
type ATemplate = `aA${string}`;
type BTemplate = Uppercase<ATemplate>;
type CTemplate = Lowercase<ATemplate>;

let aTemp: ATemplate = "aAFoo";
let bTemp: BTemplate = "AAFOO";
let cTemp: CTemplate = "aafoo";

export {};
