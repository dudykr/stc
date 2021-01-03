
const a = {
    a: 1,
    b: '2',

    foo() {
        return this[Math.random() > 0.5 ? 'a' : 'b']
    }
}