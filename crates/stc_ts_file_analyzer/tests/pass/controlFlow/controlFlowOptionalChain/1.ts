//@strict: true

// assignments in shortcutting chain
declare const o: undefined | {
    [key: string]: any;
    [key: number]: any;
    (...args: any[]): any;
};

let d: number;
o?.x(d = 1);
d.toString();

export { }