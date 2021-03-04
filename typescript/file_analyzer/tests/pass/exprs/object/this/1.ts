export let o = {
    d: "bar",
    m() {
        return this.d.length;
    },
    f: function () {
        return this.d.length;
    }
}

