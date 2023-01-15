//@strict: true

function foo1(results: number[] | undefined, results1: number[] | undefined) {
    (results ||= results1 ||= []).push(100);
}
