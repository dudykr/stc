// see 'typeRelatedToDiscriminatedType' in checker.ts:

// https://github.com/Microsoft/TypeScript/issues/30170
namespace GH30170 {
    interface Blue {
        color: 'blue'
    }
    interface Yellow {
        color?: 'yellow',
    }
    function draw(val: Blue | Yellow) { }

    function drawWithColor(currentColor: 'blue' | 'yellow' | undefined) {
        return draw({ color: currentColor });
    }
}
