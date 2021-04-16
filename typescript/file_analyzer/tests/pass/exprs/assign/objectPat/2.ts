
const sa = Symbol();
const sb = Symbol();

function f2<T extends { [sa]: string, [sb]: number }>(obj: T) {
    let { [sa]: a1, [sb]: b1, ...r1 } = obj;
}


export { }