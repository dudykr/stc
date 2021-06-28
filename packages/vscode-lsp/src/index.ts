import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
    const binaryPath = context.asAbsolutePath(path.join('..', '..', 'target', 'debug', 'stc'))

    const executable: Executable = {
        command: binaryPath,
        args: ['lsp'],
    };
    const debugOptions: Executable = {
        command: binaryPath,
        args: ['lsp'],
    };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions: ServerOptions = {
        run: executable,
        debug: debugOptions
    };

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'typescript' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'stc-lsp',
        'STC language server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    context.subscriptions.push(client.start())
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
