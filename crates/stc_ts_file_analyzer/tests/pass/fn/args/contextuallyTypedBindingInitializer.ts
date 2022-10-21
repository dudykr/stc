// @noImplicitAny: true
interface Show {
    show: (x: number) => string;
}
export function f({ show = v => v.toString() }: Show) { }