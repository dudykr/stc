//@strict: true

function c(x: number | void, y: void, z: void | string | number): void {

}

c(3, void 0, void 0); // ok
c(3, void 0); // ok
c(3); // ok
c(); // ok


export { }
