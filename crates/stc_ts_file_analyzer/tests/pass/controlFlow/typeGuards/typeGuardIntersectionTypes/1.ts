// @strictNullChecks: true

// Repro from #9016

declare function log(s: string): void;

// Supported beast features
interface Beast { wings?: boolean; legs?: number }
interface Legged { legs: number; }
interface Winged { wings: boolean; }

// Beast feature detection via user-defined type guards
declare function hasLegs(x: Beast): x is Legged;
declare function hasWings(x: Beast): x is Winged;

// Function to identify a given beast by detecting its features
function identifyBeast(beast: Beast) {

    // All beasts with legs
    if (hasLegs(beast)) {

        // All winged beasts with legs
        if (hasWings(beast)) {

        }

        // All non-winged beasts with legs
        else {
            log(`manbearpig - ${beast.legs} legs, no wings`);
        }
    }


}
