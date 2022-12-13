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
        { [(() => { foo = ""; return 'window' as const })()]: bar } } = window;

    foo;  // should be string
}