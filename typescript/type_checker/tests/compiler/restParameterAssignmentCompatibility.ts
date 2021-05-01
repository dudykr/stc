﻿class T {
    m(...p3) {

    }
}

class S {
    m(p1, p2) {

    }
}

var t: T;
var s: S;
// M is a non - specialized call or construct signature and S' contains a call or construct signature N where,
//  the number of non-optional parameters in N is less than or equal to the total number of parameters in M,
t = s; // Should be valid (rest params correspond to an infinite expansion of parameters)

class T1 {
    m(p1?, p2?) {

    }
}
var t1: T1;
// When comparing call or construct signatures, parameter names are ignored and rest parameters correspond to an unbounded expansion of optional parameters of the rest parameter element type.
t1 = s; // Similar to above, but optionality does not matter here.