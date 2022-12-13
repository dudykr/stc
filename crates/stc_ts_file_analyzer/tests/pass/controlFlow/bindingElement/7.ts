// @strictNullChecks: true
// @allowUnreachableCode: false
{
    interface Window {
        window: Window;
    }

    let foo: string | undefined;
    let window = {} as Window;
    window.window = window;

    const { [(() => { return 'window' as const })()]:
        { [(() => { return 'window' as const })()]: bar = (() => { foo = ""; return window; })() } } = window;

    foo;  // should be string | undefined
}
