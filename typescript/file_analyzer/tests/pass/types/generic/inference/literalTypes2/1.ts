
function makeArray<T>(x: T): T[] {
    return [x];
}

function append<T>(a: T[], x: T): T[] {
    let result = a.slice();
    result.push(x);
    return result;
}

type Bit = 0 | 1;

let aa = makeArray<Bit>(0);
aa = append(aa, 1);

export { }