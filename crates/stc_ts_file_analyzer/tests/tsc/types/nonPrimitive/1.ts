// @strict: true


function l<T extends {}, P extends keyof T>(s: string, tp: T[P]): void {
    tp = s;
}
function m<T extends { a: number }, P extends keyof T>(s: string, tp: T[P]): void {
    tp = s;
}
function f<T extends object, P extends keyof T>(s: string, tp: T[P]): void {
    tp = s;
}
