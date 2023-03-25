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
    let page = 0;
    while (true) {
        const issues = await octokit.rest.issues.listForRepo({
            owner: 'dudykr',
            repo: 'stc',
            creator: 'kdy1',
            labels: 'tsc-unit-test',
            per_page: 100,
            page: page++,
            state: 'open',
        });


        all.push(...issues.data);

        if (issues.data.length < 100) {
            break;
        }

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

    // console.log(allIssues)

    for (const file of files) {
        console.group(`Syncing ${file}`)

        try {
            const title = `Fix unit test:  \`${file}\``;
            const prevIssue = allIssues.find(issue => issue.title.includes(file));

            const body = `
                
    
---

Related test: https://github.com/dudykr/stc/blob/main/crates/stc_ts_file_analyzer/${file}

---

This issue is created by sync script.
        `;

            if (prevIssue) {
                console.log(`Prev issue found: ${prevIssue.number}`)
                if (prevIssue.body !== body || prevIssue.title !== title) {
                    await octokit.rest.issues.update({
                        owner: 'dudykr',
                        repo: 'stc',
                        issue_number: prevIssue.number,
                        body,
                        title,
                        labels: ['tsc-unit-test']
                    })
                }
            } else {
                const issue = await octokit.rest.issues.create({
                    owner: 'dudykr',
                    repo: 'stc',
                    title,
                    body,
                    labels: ['tsc-unit-test']
                })
            }



        } finally {
            console.groupEnd()
        }

    }
}

main();
