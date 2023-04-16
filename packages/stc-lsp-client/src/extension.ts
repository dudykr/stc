import * as path from 'path';
import {
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri
} from 'vscode';

import {
	LanguageClient, LanguageClientOptions, ServerOptions, TransportKind
} from 'vscode-languageclient/node';

let defaultClient: LanguageClient;
const clients: Map<string, LanguageClient> = new Map();


let _sortedWorkspaceFolders: string[] | undefined;
function sortedWorkspaceFolders(): string[] {
	if (_sortedWorkspaceFolders === void 0) {
		_sortedWorkspaceFolders = Workspace.workspaceFolders ? Workspace.workspaceFolders.map(folder => {
			let result = folder.uri.toString();
			if (result.charAt(result.length - 1) !== '/') {
				result = result + '/';
			}
			return result;
		}).sort(
			(a, b) => {
				return a.length - b.length;
			}
		) : [];
	}
	return _sortedWorkspaceFolders;
}
Workspace.onDidChangeWorkspaceFolders(() => _sortedWorkspaceFolders = undefined);

function getOuterMostWorkspaceFolder(folder: WorkspaceFolder): WorkspaceFolder {
	const sorted = sortedWorkspaceFolders();
	for (const element of sorted) {
		let uri = folder.uri.toString();
		if (uri.charAt(uri.length - 1) !== '/') {
			uri = uri + '/';
		}
		if (uri.startsWith(element)) {
			return Workspace.getWorkspaceFolder(Uri.parse(element))!;
		}
	}
	return folder;
}

const langServerId = 'stc-lsp';
const langServerName = 'Language server for stc';

export function activate(context: ExtensionContext) {
	const outputChannel: OutputChannel = Window.createOutputChannel('stc language server');

	const binaryFile = context.asAbsolutePath(path.join('..', '..', 'target', 'debug', 'examples', 'lsp'));
	outputChannel.appendLine(`BinaryFile: ${binaryFile}`)

	function didOpenTextDocument(document: TextDocument): void {
		// We are only interested in language mode text
		if (document.languageId !== 'typescript' || (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')) {
			return;
		}

		const serverOptions: ServerOptions = {
			run: { command: binaryFile, transport: TransportKind.ipc },
			debug: { command: binaryFile, transport: TransportKind.ipc }
		};
		const baseClientOptions: LanguageClientOptions = {
			diagnosticCollectionName: 'stc-lsp',
			outputChannel: outputChannel,
		};


		const uri = document.uri;
		// Untitled files go to a default client.
		if (uri.scheme === 'untitled' && !defaultClient) {

			const clientOptions: LanguageClientOptions = {
				...baseClientOptions,
				documentSelector: [
					{ scheme: 'untitled', language: 'typescript' }
				],
			};
			defaultClient = new LanguageClient(langServerId, langServerName, serverOptions, clientOptions);
			defaultClient.start();
			return;
		}
		let folder = Workspace.getWorkspaceFolder(uri);
		// Files outside a folder can't be handled. This might depend on the language.
		// Single file languages like JSON might handle files outside the workspace folders.
		if (!folder) {
			return;
		}
		// If we have nested workspace folders we only start a server on the outer most workspace folder.
		folder = getOuterMostWorkspaceFolder(folder);

		if (!clients.has(folder.uri.toString())) {
			const clientOptions: LanguageClientOptions = {
				...baseClientOptions,
				documentSelector: [
					{ scheme: 'file', language: 'plaintext', pattern: `${folder.uri.fsPath}/**/ * ` }
				],
				workspaceFolder: folder,
			};
			const client = new LanguageClient(langServerId, langServerName, serverOptions, clientOptions);
			client.start();
			clients.set(folder.uri.toString(), client);
		}
	}

	Workspace.onDidOpenTextDocument(didOpenTextDocument);
	Workspace.textDocuments.forEach(didOpenTextDocument);
	Workspace.onDidChangeWorkspaceFolders((event) => {
		for (const folder of event.removed) {
			const client = clients.get(folder.uri.toString());
			if (client) {
				clients.delete(folder.uri.toString());
				client.stop();
			}
		}
	});
}

export function deactivate(): Thenable<void> {
	const promises: Thenable<void>[] = [];
	if (defaultClient) {
		promises.push(defaultClient.stop());
	}
	for (const client of clients.values()) {
		promises.push(client.stop());
	}
	return Promise.all(promises).then(() => undefined);
}
