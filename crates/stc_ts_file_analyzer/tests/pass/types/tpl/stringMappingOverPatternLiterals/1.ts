// non-template
type A = "aA";
type B = Uppercase<A>;
type C = Lowercase<A>;

// templated
type ATemplate = `aA${string}`;
type BTemplate = Uppercase<ATemplate>;
type CTemplate = Lowercase<ATemplate>;

var b: BTemplate;
b = "AAFOO";

var aTemp: ATemplate;
aTemp = "aAFoo";
var bTemp: BTemplate;
bTemp = "AAFOO";
var cTemp: CTemplate;
cTemp = "aafoo";

export {};
