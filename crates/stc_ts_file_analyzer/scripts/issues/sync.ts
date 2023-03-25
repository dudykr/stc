import fs from "fs";
import path from "path";
import * as dotenv from 'dotenv'
import { Octokit, App } from "octokit";
import { GetResponseTypeFromEndpointMethod } from "@octokit/types";

async function* walk(dir: string): AsyncGenerator<string> {
    for await (const d of await fs.promises.opendir(dir)) {
        const entry = path.join(dir, d.name);
        if (d.isDirectory()) yield* walk(entry);
        else if (d.isFile()) yield entry;
    }
}

async function arrayFromGenerator<T>(gen: AsyncIterable<T>): Promise<T[]> {
    const out: T[] = []
    for await (const x of gen) {
        out.push(x)
    }
    return out
}

dotenv.config()

if (!process.env.GITHUB_TOKEN) throw new Error('GITHUB_TOKEN not set');

const octokit = new Octokit({ auth: process.env.GITHUB_TOKEN });

type IssueItems = GetResponseTypeFromEndpointMethod<
    typeof octokit.rest.issues.listForRepo
>['data'];


const fetchAllIssue = async () => {
    const all: IssueItems = [];
    while (true) {
        const issues = await octokit.rest.issues.listForRepo({
            owner: 'dudykr',
            repo: 'stc',
            creator: 'kdy1',
            labels: 'tsc-unit-test',
        });

        if (issues.data.length === 0) {
            break;
        }

        all.push(...issues.data);
    }

    return all
}



async function main() {
    const files = [
        ...await arrayFromGenerator(walk('./tests/pass-only/')),
        ...await arrayFromGenerator(walk('./tests/tsc/'))
    ].filter(filepath => path.basename(filepath).startsWith('.'));

    console.log(files);

    const allIssues = await fetchAllIssue();

    console.log(allIssues)

    for (const file of files) {

    }
}

main();
