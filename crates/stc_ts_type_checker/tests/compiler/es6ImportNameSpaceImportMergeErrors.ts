// @target: es5
// @module: commonjs

// @filename: es6ImportNameSpaceImportMergeErrors_0.ts
export var a = 10;

// @filename: es6ImportNameSpaceImportMergeErrors_1.ts
import * as nameSpaceBinding from "./es6ImportNameSpaceImportMergeErrors_0"; 
interface nameSpaceBinding { } // this should be ok

import * as nameSpaceBinding1 from "./es6ImportNameSpaceImportMergeErrors_0"; // should be error
import * as nameSpaceBinding1 from "./es6ImportNameSpaceImportMergeErrors_0"; // should be error

import * as nameSpaceBinding3 from "./es6ImportNameSpaceImportMergeErrors_0"; // should be error
var nameSpaceBinding3 = 10;
