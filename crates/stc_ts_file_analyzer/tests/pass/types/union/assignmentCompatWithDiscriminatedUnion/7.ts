// see 'typeRelatedToDiscriminatedType' in checker.ts:


// https://github.com/Microsoft/TypeScript/issues/15907
namespace GH15907 {
    type Action = { type: 'activate' } | { type: 'disactivate' };

    function dispatchAction(action: Action): void {

    }

    const active = true;

    dispatchAction({ type: (active ? 'disactivate' : 'activate') });
}
