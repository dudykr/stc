// see 'typeRelatedToDiscriminatedType' in checker.ts:

// https://github.com/Microsoft/TypeScript/issues/12052
namespace GH12052 {
    interface ILinearAxis { type: "linear"; }

    interface ICategoricalAxis { type: "categorical"; }

    type IAxis = ILinearAxis | ICategoricalAxis;
    type IAxisType = "linear" | "categorical";

    function getAxisType(): IAxisType {
        if (1 == 1) {
            return "categorical";
        } else {
            return "linear";
        }
    }

    const bad: IAxis = { type: getAxisType() };
    const good: IAxis = { type: undefined };
    good.type = getAxisType();
}

