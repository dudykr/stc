#!/usr/bin/env ts-node

import * as fs from 'fs';
import * as path from 'path';


async function* walk(dir: string): AsyncGenerator<string> {
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
            throw new Error(`invalid line found: ${str}; ${line}:${column}; code = ${code}`)
        }
        errors.push({
            line,
            column,
            code
        })
    }

    return errors;
}

async function handleTestSuite(suiteName: string) {
    const multiResultTests: string[] = [];

    const refFiles = await fs.promises.readdir(path.join('tests', 'reference'));

    for await (const p of walk(`tests/${suiteName}`)) {
        if (!p.endsWith('.ts') && !p.endsWith('.tsx')) continue;
        const dir = path.dirname(p);
        const fname = path.basename(p);
        const testName = path.parse(p).name;
        const refName = path.parse(fname).name;

        for (const refFile of refFiles) {
            if (!refFile.endsWith('.errors.txt')) {
                continue;
            }

            if (!refFile.startsWith(refName) || !testName.endsWith(refName)) {
                continue;
            }

            const errorFilePath = path.join('tests', 'reference', refFile)
            if (!fs.existsSync(errorFilePath)) {
                continue
            }
            const errorJsonPath = path.join(dir, refFile.replace('.txt', '.json'));
            if (errorJsonPath.includes('(')) {
                multiResultTests.push(p.replace(`tests/${suiteName}/`, ''));
            }

            const content = await fs.promises.readFile(errorFilePath, 'utf-8');

            console.log('----- ----- ----- ----- -----')
            console.log(p)

            console.log('Error refs:', errorFilePath)

            const errors = extract(content)
            await fs.promises.writeFile(errorJsonPath, JSON.stringify(errors))
        }
    }
    multiResultTests.sort()

    await fs.promises.writeFile(path.join('tests', `${suiteName}.multiresult.txt`), multiResultTests.join('\n'), 'utf8')
}

(async function () {
    await handleTestSuite('conformance');
    await handleTestSuite('compiler');
})()

