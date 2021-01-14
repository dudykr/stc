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

interface ErrorRef {
    line: number,
    column: number,
    code: string,
}

function extract(content: string): ErrorRef[] {
    const errors: ErrorRef[] = [];
    for (const str of content.split('\n')) {
        if (str.startsWith('====')) {
            break
        }
        if (str.startsWith(' ')) {
            continue
        }
        const [, data] = str.split('(', 2);
        if (!data) {
            continue
        }

        const [line, column] = (data.split(')', 2)[0].split(',').map(v => parseInt(v)))

        const code = data.split('error ', 2)[1]?.split(':')[0];

        console.log(line, column, code);
        if (!line || !column || !code) {
            throw new Error(`invalid line found: ${str}`)
        }
        errors.push({
            line,
            column,
            code
        })
    }

    return errors;
}

(async function () {
    for await (const p of walk('tests/conformance')) {
        if (!p.endsWith('.ts') && !p.endsWith('.tsx')) continue;
        const dir = path.dirname(p);
        const fname = path.basename(p);
        const errorFilePath = path.join('tests', 'reference', `${fname.replace('.ts', '.errors.txt')}`)
        if (!fs.existsSync(errorFilePath)) {
            continue
        }
        const nameWithoutExt = fname.split('.').slice(0, -1).join('.');
        const errorJsonPath = path.join(dir, `${nameWithoutExt}.errors.json`);

        const content = await fs.promises.readFile(errorFilePath, 'utf-8');
        const errors = extract(content)

        console.log('----- ----- ----- ----- -----')
        console.log(p)

        console.log('Error refs:', errorFilePath)
        await fs.promises.writeFile(errorJsonPath, JSON.stringify(errors))
    }
})()

