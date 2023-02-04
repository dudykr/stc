//@strict:true

// Repro from #48468

export function deepEquals<T>(a: T, b: T): boolean {
    if (typeof a !== 'object' || typeof b !== 'object' || !a || !b) {
        return false;
    }
    if (Array.isArray(a) || Array.isArray(b)) {
        return false;
    }
    if (Object.keys(a).length !== Object.keys(b).length) { // Error here
        return false;
    }
    return true;
}
