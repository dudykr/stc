// see 'typeRelatedToDiscriminatedType' in checker.ts:


// https://github.com/Microsoft/TypeScript/issues/20889
namespace GH20889 {
    interface A1 {
        type: "A1";
    }
    interface A2 {
        type: "A2";
    }
    type AU = A1 | A2;

    function foo(obj1: AU) {
        const obj2: AU = {
            type: obj1.type
        };
    }
}
