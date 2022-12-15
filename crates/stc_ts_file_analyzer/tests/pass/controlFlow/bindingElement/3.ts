// @strictNullChecks: true
// @allowUnreachableCode: false
{
    const data = { param: 'value' };

    let foo: string | undefined = "";
    const {
        param = (() => { foo = undefined })(),
    } = data;

    foo;  // should be string | undefined
}