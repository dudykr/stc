class C { }

class D { }

function f2<T extends C, U extends D>(v: T | U) {
    if (v instanceof C) {
        const x: T = v;
    }
    else {
        const y: U = v;
    }
}

export { }