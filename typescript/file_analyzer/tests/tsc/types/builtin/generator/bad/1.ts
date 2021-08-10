interface BadGenerator extends Iterator<number>, Iterable<string> { }
export function* g3(): BadGenerator { }