
export function foo() {
    let errors: any[] | undefined;

    errors = errors ?? []

    // Verify that the type of errors is `any[]`
    const arr: any[] = [];
    arr.push(...errors);

    return errors
}