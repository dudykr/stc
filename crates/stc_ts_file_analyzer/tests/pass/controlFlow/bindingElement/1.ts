// @strictNullChecks: true
// @allowUnreachableCode: false
{
    const data = { param: 'value' };

    const {
        param = (() => { throw new Error('param is not defined') })(),
    } = data;

    console.log(param); // should not trigger 'Unreachable code detected.'    
}


