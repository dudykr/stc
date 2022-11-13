// non-template
type A = "aA";
type B = Uppercase<A>;
type C = Lowercase<A>;

// templated
type ATemplate = `aA${string}`;
type BTemplate = Uppercase<ATemplate>;
type CTemplate = Lowercase<ATemplate>;

var a: ATemplate
var b: BTemplate
var c: CTemplate

a = 'aAFoo'
b = 'AAFOO'
c = 'aafoo'

export { }