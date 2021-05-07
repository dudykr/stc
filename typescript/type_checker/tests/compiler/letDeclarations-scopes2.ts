// @target: ES6

let global = 0;

{ 
    let local = 0;

    local;  // OK
    global; // OK
    local2; // Error

    {
        let local2 = 0;

        local;  // OK
        global; // OK
        local2; // OK
    } 

    local;  // OK
    global; // OK
    local2; // Error
}

local;  // Error
global; // OK
local2; // Error
