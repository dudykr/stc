#!/usr/bin/env node

import * as fs from 'fs';
import * as path from 'path';


async function* walk(dir: string): any {
    for await (const d of await fs.promises.opendir(dir)) {
        const entry = path.join(dir, d.name);
        if (d.isDirectory()) yield* walk(entry);
        else if (d.isFile()) yield entry;
    }
}


(async function () {
    for await (const p of walk('tests/conformance')) {
        if (!p.endsWith('.ts') && !p.endsWith('.tsx')) continue;
        const fname = path.basename(p);
        const errorFilePath = path.join('tests', 'reference', `${fname.replace('.ts', '.errors.txt')}`)
        if (!fs.existsSync(errorFilePath)) {
            continue
        }

        console.log('----- ----- ----- ----- -----')
        console.log(p)

        console.log('Error refs:', errorFilePath)

    }
})()

