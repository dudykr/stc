// @strictNullChecks: true
// @allowUnreachableCode: false
{
    const data = { param: 'value' };

    let foo: string | undefined = "";
    const {
        param = (() => { return "" + 1 })(),
    } = data;

    foo;  // should be string
}