// (arg?: { x: number, y?: number }) => void
export function f5({ x, y = 0 } = { x: 0 }) { }
f5();
f5({ x: 1 });
f5({ x: 1, y: 1 });
