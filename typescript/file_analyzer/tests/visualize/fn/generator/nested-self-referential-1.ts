function* foo() {
    function bar() {
        function* quux() {
            yield (foo);
        }
    }
}

export { }