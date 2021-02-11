
declare var x: number | string | boolean;

// Mixing typeguard narrowing in if statement with conditional expression typeguard
// Assigning value to x in outer guard shouldn't stop narrowing in the inner expression
if (typeof x === "string") {

} else {
    x = 10;
    var b = x; // number | boolean | string
    b
}

export { }

