

interface Shape {
    name: string;
    width: number;
    height: number;
}



declare var m: Partial<Shape | { name: 'circle' }>;

declare var test1: { 'width': any }
declare var test2: { 'name': string }
m

m = test1
m = test2