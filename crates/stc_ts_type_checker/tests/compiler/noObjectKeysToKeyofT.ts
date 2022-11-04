// @target: es2016

// Do not change Object.keys to return keyof T.
// The current return type (string[]) is intentional.
Object.keys({ a: 0 }).push("b");

// See also
// https://stackoverflow.com/questions/55012174/why-doesnt-object-keys-return-a-keyof-type-in-typescript