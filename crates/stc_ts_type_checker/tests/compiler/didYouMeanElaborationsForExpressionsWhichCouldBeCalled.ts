class Bar {
    x!: string;
}

declare function getNum(): number;

declare function foo(arg: { x: Bar, y: Date }, item: number, items?: [number, number, number]): void;

foo({
    x: Bar,
    y: Date
}, getNum());

foo({
    x: new Bar(),
    y: new Date()
}, getNum);


foo({
    x: new Bar(),
    y: new Date()
}, getNum(), [
    1,
    2,
    getNum
]);
