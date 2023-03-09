function f00([x, y]) {}
function f01([x, y] = []) {}
function f02([x, y] = [1]) {}
function f03([x, y] = [1, 'foo']) {}

function f10([x = 0, y]) {}
function f11([x = 0, y] = []) {}
function f12([x = 0, y] = [1]) {}
function f13([x = 0, y] = [1, 'foo']) {}

function f20([x = 0, y = 'bar']) {}
function f21([x = 0, y = 'bar'] = []) {}
function f22([x = 0, y = 'bar'] = [1]) {}
function f23([x = 0, y = 'bar'] = [1, 'foo']) {}

declare const nx: number;
declare const sx: unknown;

function f30([x = 0, y = 'bar']) {}
function f31([x = 0, y = 'bar'] = []) {}
function f32([x = 0, y = 'bar'] = [nx]) {}
function f33([x = 0, y = sx] = [nx]) {}

// function f40([x = 0, y = 'bar']) {}
// function f41([x = 0, y = 'bar'] = []) {}
// function f42([x = 0, y = 'bar'] = [sx]) {}
// function f43([x = 0, y = 'bar'] = [sx, nx]) {}
f00([1, 'a']);
f01();
f02();
f03();

f10([1, 'a']);
f11();
f12();
f13();

f20([1, 'a']);
f21();
f22();
f23();

f30([1, 'a']);
f31();
f32();
f33();
