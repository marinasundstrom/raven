import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

function resolveServerPath(context: vscode.ExtensionContext): string {
  const configuration = vscode.workspace.getConfiguration('raven');
  const configuredPath = configuration.get<string>('languageServerPath')?.trim();

  if (configuredPath && fs.existsSync(configuredPath)) {
    return configuredPath;
  }

  const packagedPath = context.asAbsolutePath(path.join('server', 'Raven.LanguageServer.dll'));
  if (fs.existsSync(packagedPath)) {
    return packagedPath;
  }

  const workspacePath = path.join(
    context.extensionPath,
    '..',
    'Raven.LanguageServer',
    'bin',
    'Debug',
    'net9.0',
    'Raven.LanguageServer.dll'
  );

  if (fs.existsSync(workspacePath)) {
    return workspacePath;
  }

  throw new Error(
    'Unable to locate Raven.LanguageServer.dll. Build the language server or set "raven.languageServerPath" to the compiled DLL.'
  );
}

export function activate(context: vscode.ExtensionContext): void {
  const serverPath = resolveServerPath(context);

  const runCommand = {
    command: 'dotnet',
    args: [serverPath],
    options: {
      cwd: path.dirname(serverPath)
    }
  };

  const serverOptions: ServerOptions = {
    run: runCommand,
    debug: runCommand
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'raven' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.rav')
    },
    outputChannel: vscode.window.createOutputChannel('Raven Language Server')
  };

  client = new LanguageClient(
    'ravenLanguageServer',
    'Raven Language Server',
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(client);
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
