class class1 {
    public prop1 = {
        doStuff: (callback) => () => {
            var _this = 2;
            return callback(_this);
        }
    }
}

class class2 {
    constructor() {
        var _this = 2;
    }
    public prop1 = {
        doStuff: (callback) => () => {
            return callback(10);
        }
    }
}