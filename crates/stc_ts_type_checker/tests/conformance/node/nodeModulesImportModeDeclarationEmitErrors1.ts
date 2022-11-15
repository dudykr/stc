// @noImplicitReferences: true
// @module: node16,nodenext
// @declaration: true
// @outDir: out
// @filename: /node_modules/pkg/package.json
{
    "name": "pkg",
    "version": "0.0.1",
    "exports": {
        "import": "./import.js",
        "require": "./require.js"
    }
}
// @filename: /node_modules/pkg/import.d.ts
export interface ImportInterface {}
// @filename: /node_modules/pkg/require.d.ts
export interface RequireInterface {}
// @filename: /index.ts
// incorrect mode
import type { RequireInterface } from "pkg" assert { "resolution-mode": "foobar" };
// not type-only
import { ImportInterface } from "pkg" assert { "resolution-mode": "import" };
// not exclusively type-only
import {type RequireInterface as Req, RequireInterface as Req2} from "pkg" assert { "resolution-mode": "require" };

export interface LocalInterface extends RequireInterface, ImportInterface {}



