// @strict: true
function stringify1(anything: { toString(): string } | undefined): string {
    return typeof anything === "string" ? anything.toUpperCase() : "";
}

function stringify2(anything: {} | undefined): string {
    return typeof anything === "string" ? anything.toUpperCase() : "";
}

function stringify3(anything: unknown | undefined): string { // should simplify to just `unknown` which should narrow fine
    return typeof anything === "string" ? anything.toUpperCase() : "";
}

function stringify4(anything: { toString?(): string } | undefined): string {
    return typeof anything === "string" ? anything.toUpperCase() : "";
}
