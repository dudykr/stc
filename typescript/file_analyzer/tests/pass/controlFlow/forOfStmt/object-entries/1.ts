var o = { a: 1, b: 2 };

for (var x of Object.values(o)) {
    let y = x;
}

export var entries = Object.entries(o);