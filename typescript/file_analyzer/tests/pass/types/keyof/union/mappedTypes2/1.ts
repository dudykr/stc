

interface Shape {
    name: string;
    width: number;
    height: number;
    location: Point;
}

type K = keyof (Shape | {
    name: "circle";
});


declare var k: K

k