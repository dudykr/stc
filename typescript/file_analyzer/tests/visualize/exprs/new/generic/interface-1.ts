interface I {
    id: number;
}

class C implements I {
    id: number;
}

export var anInterface: I = new C()
