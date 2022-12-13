// @strictNullChecks: true
// @allowUnreachableCode: false
{
    const data = { param: 'value' };

    let foo: string | undefined = "";
    const {
        param = (() => { throw new Error('param is not defined') })(),
    } = data;

    foo;  // should be string  
}