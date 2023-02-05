//@allowUnreachableCode: true

// Two mutually recursive function implementations with return type annotation in one
function rec3(): number {
    return rec4();
}
function rec4() {
    return rec3();
}

export { }
