function foo(arg1, arg2, callback:(v1,v2,v3) => void):void {
                return callback(arg1, arg2, arg2);
}