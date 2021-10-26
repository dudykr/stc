// FunctionExpression f with no return type annotation and directly references f in its body returns any
export var a: any = function f() {
    return f;
};