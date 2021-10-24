type ZeroOf<T extends number | string | boolean> = T extends number ? 0 : T extends string ? "" : false;

function zeroOf<T extends number | string | boolean>(value: T) {
    return <ZeroOf<T>>(typeof value === "number" ? 0 : typeof value === "string" ? "" : false);
}

function f21<T extends number | string>(x: T, y: ZeroOf<T>) {
    let z1: number | string = y;
    let z2: 0 | "" = y;
}