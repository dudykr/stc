import * as path from 'path';
import { ExtensionContext, StatusBarAlignment, window, workspace } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

export async function activate(context: ExtensionContext) {
    // VS Code doesn't show a notification when an extension fails to activate
    // so we do it ourselves.
    await tryActivate(context).catch(err => {
        window.showErrorMessage(`Failed to activate stc-lang-server: ${err.message}`);
        throw err;
    });
}

async function tryActivate(context: ExtensionContext) {
    const statusBar = window.createStatusBarItem(StatusBarAlignment.Left);
    context.subscriptions.push(statusBar);
    statusBar.text = "stc";
    statusBar.tooltip = "ready";
    statusBar.show();


    // The server is implemented in node
    let serverExecutablePath = context.asAbsolutePath(
        path.join('..', '..', 'target', 'debug', 'stc-ts-lang-server')
    );
    const clientOutputChannel = window.createOutputChannel('STC Language Server Client');
    context.subscriptions.push(clientOutputChannel);
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions: ServerOptions = {
        run: { command: serverExecutablePath },
        debug: {
            command: serverExecutablePath
        }
    };

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [
            { scheme: 'file', language: 'javascript' },
            { scheme: 'file', language: 'typecript' }
        ],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'stcLangServer',
        'STC Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}
