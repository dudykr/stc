var strOrNumOrBool: string | number | boolean;
var numOrBool: number | boolean;

// (typeguard1) || simpleExpr
if (typeof strOrNumOrBool === "string" || numOrBool !== strOrNumOrBool) {
    var r3: string | number | boolean = strOrNumOrBool; // string | number | boolean
    r3;
}
else {
    numOrBool = strOrNumOrBool; // number | boolean
}