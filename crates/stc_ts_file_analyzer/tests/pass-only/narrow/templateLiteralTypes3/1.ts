
type Action =
    | { type: `${string}_REQUEST` }
    | { type: `${string}_SUCCESS`, response: string };

export function reducer(action: Action) {
    if (action.type === 'FOO_SUCCESS') {
        action.type;
        action.response;
    }
}
