interface I<T, U> {
    tuple1: [T, U];
}

var i1: I<string, number>;
var i2: I<{}, {}>;

// no error
i1.tuple1 = ["foo", 5, false, true];