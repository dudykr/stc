function hasKind(entity: Entity, kind: "A"): entity is A;
function hasKind(entity: Entity, kind: "B"): entity is B;
function hasKind(entity: Entity, kind: Kind): entity is Entity;
function hasKind(entity: Entity, kind: Kind): boolean {
    return entity.kind === kind;
}

let x: A = {
    kind: "A",
    a: 100,
}

if (!hasKind(x, "B")) {
    let c = x;
}

export { }