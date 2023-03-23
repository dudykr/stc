let cond: boolean;

export function b() {
    let x: string | number | boolean;
    for (x = 5; cond; x = x.length) {
        x; // number
        x = "";
    }
}
