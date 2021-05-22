
class C {
    prop: string;
}

function fun<T>(item: { [P in keyof T]: T[P] }) {
    const strings: string[] = [];
    for (const key in item) {
        const value = item[key];
        if (typeof value === "string") {
            strings.push(value);
        }
    }
}

export { }