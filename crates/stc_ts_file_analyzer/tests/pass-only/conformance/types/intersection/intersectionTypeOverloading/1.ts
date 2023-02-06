// Check that order is preserved in intersection types for purposes of
// overload resolution

type F = (s: string) => string;
type G = (x: any) => any;

var fg: F & G;

var x = fg("abc");
var x: string;


export { x }