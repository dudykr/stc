// @typeScriptVersion: 5.0
// @filename: /foo/tsconfig.json
{
    "compilerOptions": {
        "module": "amd",
        "target": "ES3",
        "noImplicitUseStrict": true,
        "keyofStringsOnly": true,
        "suppressExcessPropertyErrors": true,
        "suppressImplicitAnyIndexErrors": true,
        "noStrictGenericChecks": true,
        "charset": "utf8",
        "out": "dist.js",
        "ignoreDeprecations": "5.0"
    }
}

// @filename: /foo/a.ts
const a = 1;
