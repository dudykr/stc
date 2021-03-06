var o: {
    prop1: number | string;
} = {
    prop1: "string",
}

if (typeof o.prop1 === "string" && o.prop1.toLowerCase()) { }

export { }