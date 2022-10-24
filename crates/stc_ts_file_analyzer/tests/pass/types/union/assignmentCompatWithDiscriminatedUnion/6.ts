// see 'typeRelatedToDiscriminatedType' in checker.ts:


// https://github.com/Microsoft/TypeScript/issues/18421
namespace GH18421 {
    interface ThingTypeOne {
        type: 'one';
    }

    interface ThingTypeTwo {
        type: 'two';
    }

    type ThingType = 'one' | 'two';

    type Thing = ThingTypeOne | ThingTypeTwo;

    function makeNewThing(thingType: ThingType): Thing {
        return {
            type: thingType
        };
    }
}
