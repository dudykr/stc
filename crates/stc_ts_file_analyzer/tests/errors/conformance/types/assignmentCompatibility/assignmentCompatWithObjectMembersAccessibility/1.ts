// members N and M of types S and T have the same name, same accessibility, same optionality, and N is assignable M


// targets
class Base {
}

interface I extends Base {
}

var i: I;

// sources
class D {
    public foo: string;
}

class E {
}

var e: E;
e = i; // errror

export { }