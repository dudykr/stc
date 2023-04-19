// @jsx: react-jsx
// @target: es2015,es2018,esnext
// @filename: test.tsx
/// <reference path="/.lib/react16.d.ts" />

export function T1(a: any) {
    return <div className={"T1"} { ...a }>T1</div>;
}

export function T2(a: any, b: any) {
    return <div className={"T2"} { ...a } { ...b }>T2</div>;
}

export function T3(a: any, b: any) {
    return <div { ...a } className={"T3"} { ...b }>T3</div>;
}

export function T4(a: any, b: any) {
    return <div className={"T4"} { ...{ ...a, ...b } }>T4</div>;
}

export function T5(a: any, b: any, c: any, d: any) {
    return <div className={"T5"} { ...{ ...a, ...b, ...{ c, d } } }>T5</div>;
}

export function T6(a: any, b: any, c: any, d: any) {
    return <div className={"T6"} { ...{ ...a, ...b, ...{ ...c, ...d } } }>T6</div>;
}

export function T7(a: any, b: any, c: any, d: any) {
    return <div>T7</div>;
}

export function T8(a: any, b: any, c: any, d: any) {
    return <div className={"T8"} { ...{ __proto__: null, dir: 'rtl' } }>T8</div>;
}

export function T9(a: any, b: any, c: any, d: any) {
    return <div className={"T9"} { ...{ "__proto__": null } }>T9</div>;
}

declare const __proto__: string;

export function T10(a: any, b: any, c: any, d: any) {
    return <div className={"T10"} { ...{ [__proto__]: null } }>T10</div>;
}

export function T11(a: any, b: any, c: any, d: any) {
    return <div className={"T11"} { ...{ ["__proto__"]: null } }>T11</div>;
}

export function T12(a: any, b: any, c: any, d: any) {
    return <div className={"T12"} { ...{ __proto__ } }>T12</div>;
}
