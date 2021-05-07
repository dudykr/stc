#!/usr/bin/env ts-node

// List short tests.
//
// Example usage: ./scripts/simple.ts >> tests/conformance.pass.txt

import * as path from 'path';
import * as fs from 'fs';

async function* walk(dir: string): AsyncGenerator<string> {
    for await (const d of await fs.promises.opendir(dir)) {
        const entry = path.join(dir, d.name);
        if (d.isDirectory()) yield* walk(entry);
        else if (d.isFile()) yield entry;
    }
}

const conformancePassListPath = path.join(__dirname, '..', 'tests', 'conformance.pass.txt');


const conformanceDir = path.join(__dirname, '..', 'tests', 'conformance');
console.log(`Conformance dir: ${conformanceDir}`)


async function main() {
    const passingTests = (await fs.promises.readFile(conformancePassListPath, 'utf8')).split('\n').filter(s => !!s);

    for await (const file of walk(conformanceDir)) {
        // We don't care about non-ts files.
        if (!file.endsWith('.ts')) {
            continue;
        }

        // We don't care about tests already passing.
        if (passingTests.some(testName => file.includes(testName))) {
            continue;
        }

        const src = await fs.promises.readFile(file, 'utf8');
        const lineCount = src.split('\n').length;

        if (lineCount <= 10) {
            const relPath = path.relative(conformanceDir, file);
            console.log(`${relPath}`)
        }

    }

}

main()