// non-template

// templated
type ATemplate = `aA${string}`;
type BTemplate = Uppercase<ATemplate>;

var b: BTemplate

b = 'AAFOO'

export { }