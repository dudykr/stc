var bool: boolean;
var strOrNum: string | number;
var strOrNumOrBool: string | number | boolean;


if (typeof strOrNumOrBool === "string" || typeof strOrNumOrBool === "number") {
    strOrNum = strOrNumOrBool; // string | number
}
else {
    bool = strOrNumOrBool; // boolean
}

export { }