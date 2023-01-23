//@strict: true

// Repro from #35842

interface SomeObject {
    someProperty: unknown;
}

let lastSomeProperty: unknown | undefined;

function someFunction(someOptionalObject: SomeObject | undefined): void {
    if (someOptionalObject?.someProperty !== lastSomeProperty) {
        console.log(someOptionalObject);
        lastSomeProperty = someOptionalObject?.someProperty;
    }
}

const someObject: SomeObject = {
    someProperty: 42
};

someFunction(someObject);
someFunction(undefined);

export { }