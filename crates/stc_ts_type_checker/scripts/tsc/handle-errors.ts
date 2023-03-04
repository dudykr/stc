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
    filename: string | undefined
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
        if (str.startsWith(' ') || str.startsWith('lib.') || str.includes('error TS-')) {
            continue
        }
        const [filename, data] = str.split('(', 2);
        if (!data) {
            if (str.startsWith('error ')) {
                const code = str.substring(6).split(':')[0];
                errors.push({
                    filename: undefined,
                    line: 0,
                    column: 0,
                    code
                })
            }
            continue
        }

        const [line, column] = (data.split(')', 2)[0].split(',').map(v => parseInt(v)))

        const [, type, description] = data.split(/(error|message) /, 3);
        const code = description?.split(':')[0];
        if (type === 'message') {
            continue
        }

        console.log(line, column, code);
        if (!line || !column || !code) {
            throw new Error(`invalid line found: ${str}; ${line}:${column}; code = ${code}`)
        }
        errors.push({
            filename,
            line,
            column,
            code
        })
    }

    return errors;
}

async function handleTestSuite(suiteName: string) {

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

            const content = await fs.promises.readFile(errorFilePath, 'utf-8');
            if (content.toLowerCase().includes('[0m:')) {
                continue
            }

            console.log('----- ----- ----- ----- -----')
            console.log(p)


            console.log('Error refs:', errorFilePath)

            try {
                const errors = extract(content)

                if (p.includes('generatorReturnTypeFallback.2') && errors.length === 0) {
                    throw new Error(`ERROR(${errors})`)
                }
                await fs.promises.writeFile(errorJsonPath, JSON.stringify(errors))
            } catch (e) {
                console.error('failed to extract errors from', errorFilePath)
                throw e
            }
        }
    }
}

(async function () {
    await handleTestSuite('conformance');
    await handleTestSuite('compiler');
})()

