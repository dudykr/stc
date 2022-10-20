// 3.8.4 Assignment Compatibility 

interface Callable {
    call(blah: any); // also works for 'apply'
}

var x: Callable;

// Should fail
x = '';
x = [''];
x = 4;
x = {};

// Should work
function f() { };
x = f;

function fn(c: Callable) { }

// Should Fail
fn('');
fn(['']);
fn(4);
fn({});


// Should work
fn(a => { });
