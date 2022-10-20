export let o = {
    d: "bar",
    m() {
        return this.foo.length;
    },
    f: function () {
        return this.d.length;
    }
}

