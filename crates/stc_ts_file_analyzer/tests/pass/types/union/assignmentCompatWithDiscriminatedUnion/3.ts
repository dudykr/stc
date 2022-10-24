// see 'typeRelatedToDiscriminatedType' in checker.ts:

// https://github.com/Microsoft/TypeScript/issues/14865
namespace GH14865 {
    type Style1 = {
        type: "A";
        data: string;
    } | {
        type: "B";
        data: string;
    };

    type Style2 = {
        type: "A" | "B";
        data: string;
    }

    const a: Style2 = { type: "A", data: "whatevs" };
    let b: Style1;
    a.type; // "A" | "B"
    b.type; // "A" | "B"
    b = a; // should be assignable
}
