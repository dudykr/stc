type Kind = "A" | "B"

interface Entity {
    kind: Kind;
}

interface A extends Entity {
    kind: "A";
    a: number;
}

interface B extends Entity {
    kind: "B";
    b: string;
}

function hasKind(entity: Entity, kind: "A"): entity is A;
function hasKind(entity: Entity, kind: "B"): entity is B;
function hasKind(entity: Entity, kind: Kind): entity is Entity;
function hasKind(entity: Entity, kind: Kind): boolean {
    return entity.kind === kind;
}


hasKind