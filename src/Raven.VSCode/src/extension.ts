import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

function resolveServerPath(context: vscode.ExtensionContext, output: vscode.OutputChannel): string {
  const configuration = vscode.workspace.getConfiguration('raven');
  const configuredPath = configuration.get<string>('languageServerPath')?.trim();

  const attempts: string[] = [];

  if (configuredPath) {
    attempts.push(configuredPath);
    if (fs.existsSync(configuredPath)) {
      return configuredPath;
    }
  }

  // 1) Packaged copy: <extension>/server/Raven.LanguageServer.dll
  const packagedPath = context.asAbsolutePath(path.join('server', 'Raven.LanguageServer.dll'));
  attempts.push(packagedPath);
  if (fs.existsSync(packagedPath)) {
    return packagedPath;
  }

  // 2) Dev/workspace copy next to the extension folder
  // <repo>/src/Raven.LanguageServer/bin/{Debug|Release}/{tfm}/Raven.LanguageServer.dll
  const repoCandidateRoots = [
    path.join(context.extensionPath, '..', 'Raven.LanguageServer', 'bin'),
    path.join(context.extensionPath, '..', '..', 'Raven.LanguageServer', 'bin')
  ];

  const configurations = ['Debug', 'Release'];
  const tfms = ['net9.0', 'net8.0', 'net7.0'];

  for (const root of repoCandidateRoots) {
    for (const cfg of configurations) {
      for (const tfm of tfms) {
        const candidate = path.join(root, cfg, tfm, 'Raven.LanguageServer.dll');
        attempts.push(candidate);
        if (fs.existsSync(candidate)) {
          return candidate;
        }
      }
    }
  }

  output.appendLine('Failed to locate Raven.LanguageServer.dll. Tried:');
  for (const p of attempts) output.appendLine(`- ${p}`);

  throw new Error(
    'Unable to locate Raven.LanguageServer.dll. Build the language server or set "raven.languageServerPath" to the compiled DLL.'
  );
}

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel('Raven Language Server');
  output.appendLine('Activating Raven VS Code extension…');

  let serverPath: string;
  try {
    serverPath = resolveServerPath(context, output);
  } catch (e) {
    const message = e instanceof Error ? e.message : String(e);
    output.appendLine(message);
    output.show(true);
    void vscode.window.showErrorMessage(`Raven: ${message}`);
    return;
  }

  output.appendLine(`Using language server: ${serverPath}`);

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
      configurationSection: 'raven',
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.rav')
    },
    outputChannel: output
  };

  client = new LanguageClient(
    'ravenLanguageServer',
    'Raven Language Server',
    serverOptions,
    clientOptions
  );

  // Ensure VS Code disposes the client on shutdown.
  context.subscriptions.push({
    dispose: () => {
      // stop() is async; VS Code accepts a Thenable from dispose().
      return client?.stop();
    }
  });

  // Start the language client.
  void client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
