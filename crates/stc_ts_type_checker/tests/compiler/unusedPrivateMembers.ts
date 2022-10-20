//@noUnusedLocals:true
//@noUnusedParameters:true
//@target:ES5

class Test1 {
    private initializeInternal() {
    }

    public test() {
        var x = new Test1();
        x.initializeInternal();
    }
}

class Test2 {
    private p = 0;
    public test() {
        var x = new Test2();
        x.p;
    }
}

class Test3 {
    private get x () {
        return 0;
    }

    public test() {
        var x = new Test3();
        x.x;
    }
}

class Test4 {
    private set x(v) {
        v;
    }

    public test() {
        var x = new Test4();
        x.x;
    }
}

class Test5<T> {
    private p: T;
    public test() {
        var x = new Test5<number>();
        x.p;
    }
}

class Test6 {
    private get a() {
        return 0;
    }
    private set a(v) {
        v;
    }
    private b = 0;

    public test() {
        var x = new Test6();
        x.a++;
        x.b++;
    }
}
