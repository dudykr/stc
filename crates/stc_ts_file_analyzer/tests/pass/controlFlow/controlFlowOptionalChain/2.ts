//@strict: true


// Repros from #34570

type Shape =
    | { type: 'rectangle', width: number, height: number }
    | { type: 'circle', radius: number }

export function getArea(shape?: Shape) {
    switch (shape?.type) {
        case 'circle':
            return Math.PI * shape.radius ** 2
        case 'rectangle':
            return shape.width * shape.height
        default:
            return 0
    }
}
