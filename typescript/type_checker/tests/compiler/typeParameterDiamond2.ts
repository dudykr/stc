function diamondTop<Top>() {
    function diamondMiddle<T extends Top, U>() {
        function diamondBottom<Bottom extends T | U>() {
            var top: Top;
            var middle: T | U;
            var bottom: Bottom;

            top = middle;
            middle = bottom;
            top = bottom;
        }
    }
}