// members N and M of types S and T have the same name, same accessibility, same optionality, and N is assignable M

module TargetIsPublic {
    // targets
    class Base {
        public foo: string;
    }

    interface I {
        foo: string;
    }

    class E {
        private foo: string;
    }

}

module TargetIsPublic {
    // targets
    class Base {
        private foo: string;
    }

    interface I extends Base {
    }

    var i: I;

    // sources
    class E {
        private foo: string;
    }

    var e: E;

    e = i; // errror


}

