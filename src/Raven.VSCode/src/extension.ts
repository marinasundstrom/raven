import * as fs from 'fs';
import * as path from 'path';
import { execFile } from 'child_process';
import { promisify } from 'util';
import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
const execFileAsync = promisify(execFile);
const output = vscode.window.createOutputChannel('Raven');

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

function resolveCompilerProjectPath(): string | undefined {
  const configuration = vscode.workspace.getConfiguration('raven');
  const configuredPath = configuration.get<string>('compilerProjectPath')?.trim();
  if (configuredPath) {
    const absolutePath = path.isAbsolute(configuredPath)
      ? configuredPath
      : path.resolve(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '', configuredPath);
    return fs.existsSync(absolutePath) ? absolutePath : undefined;
  }

  const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  if (!workspaceFolder) {
    return undefined;
  }

  const defaultPath = path.join(workspaceFolder, 'src', 'Raven.Compiler', 'Raven.Compiler.csproj');
  return fs.existsSync(defaultPath) ? defaultPath : undefined;
}

function resolveTargetFramework(): string | undefined {
  const configuration = vscode.workspace.getConfiguration('raven');
  const configuredFramework = configuration.get<string>('targetFramework')?.trim();
  return configuredFramework && configuredFramework.length > 0 ? configuredFramework : undefined;
}

function isRavenFile(filePath: string): boolean {
  const ext = path.extname(filePath).toLowerCase();
  return ext === '.rav' || ext === '.ravenproj';
}

function resolveDebugTarget(config: vscode.DebugConfiguration): string | undefined {
  const folder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  const configuredTarget = typeof config.target === 'string' ? config.target.trim() : '';
  const configuredProject = typeof config.project === 'string' ? config.project.trim() : '';
  const candidate = configuredProject || configuredTarget;

  if (candidate.length > 0) {
    return path.isAbsolute(candidate) ? candidate : path.resolve(folder ?? '', candidate);
  }

  const activeDocument = vscode.window.activeTextEditor?.document;
  if (activeDocument && activeDocument.uri.scheme === 'file' && isRavenFile(activeDocument.fileName)) {
    return activeDocument.fileName;
  }

  return undefined;
}

function getProjectAssemblyName(projectFilePath: string): string {
  const fallback = path.basename(projectFilePath, path.extname(projectFilePath));
  const xml = fs.readFileSync(projectFilePath, 'utf8');
  const assemblyNameMatch = xml.match(/<AssemblyName>\s*([^<]+)\s*<\/AssemblyName>/i);
  const outputAttributeMatch = xml.match(/\bOutput\s*=\s*"([^"]+)"/i);
  return assemblyNameMatch?.[1]?.trim() || outputAttributeMatch?.[1]?.trim() || fallback;
}

async function compileForDebug(targetPath: string): Promise<{ outputDllPath: string; cwd: string }> {
  const compilerProjectPath = resolveCompilerProjectPath();
  if (!compilerProjectPath) {
    throw new Error(
      'Unable to locate Raven.Compiler.csproj. Set "raven.compilerProjectPath" to continue.'
    );
  }

  const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? path.dirname(targetPath);
  const debugOutputDirectory = path.join(workspaceFolder, '.raven-debug');
  fs.mkdirSync(debugOutputDirectory, { recursive: true });
  const targetFramework = resolveTargetFramework();

  const targetIsProject = path.extname(targetPath).toLowerCase() === '.ravenproj';
  const outputArg = targetIsProject
    ? debugOutputDirectory
    : path.join(
      debugOutputDirectory,
      `${path.basename(targetPath, path.extname(targetPath))}.dll`
    );

  const dotnetArgs = [
    'run',
    ...(targetFramework ? ['--framework', targetFramework] : []),
    '--project',
    compilerProjectPath,
    '--property',
    'WarningLevel=0',
    '--',
    targetPath,
    ...(!targetIsProject ? ['--publish'] : []),
    '-o',
    outputArg,
    ...(targetFramework ? ['--framework', targetFramework] : [])
  ];

  output.appendLine(`Compiling for debug: dotnet ${dotnetArgs.join(' ')}`);

  try {
    const { stdout, stderr } = await execFileAsync('dotnet', dotnetArgs, {
      cwd: workspaceFolder,
      maxBuffer: 10 * 1024 * 1024
    });

    if (stdout.trim().length > 0) {
      output.appendLine(stdout);
    }
    if (stderr.trim().length > 0) {
      output.appendLine(stderr);
    }
  } catch (error) {
    const e = error as Error & { stdout?: string; stderr?: string };
    if (e.stdout) output.appendLine(e.stdout);
    if (e.stderr) output.appendLine(e.stderr);
    throw new Error(`Raven compile failed. See the Raven output channel for details. ${e.message}`);
  }

  const outputDllPath = targetIsProject
    ? path.join(debugOutputDirectory, `${getProjectAssemblyName(targetPath)}.dll`)
    : outputArg;

  if (!fs.existsSync(outputDllPath)) {
    throw new Error(`Compiled assembly not found at '${outputDllPath}'.`);
  }

  return { outputDllPath, cwd: path.dirname(targetPath) };
}

async function buildTarget(targetPath: string): Promise<{ outputPath: string; cwd: string }> {
  const compilerProjectPath = resolveCompilerProjectPath();
  if (!compilerProjectPath) {
    throw new Error(
      'Unable to locate Raven.Compiler.csproj. Set "raven.compilerProjectPath" to continue.'
    );
  }

  const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? path.dirname(targetPath);
  const buildOutputDirectory = path.join(workspaceFolder, '.raven-build');
  fs.mkdirSync(buildOutputDirectory, { recursive: true });
  const targetFramework = resolveTargetFramework();

  const targetIsProject = path.extname(targetPath).toLowerCase() === '.ravenproj';
  const outputArg = targetIsProject
    ? path.join(buildOutputDirectory, path.basename(targetPath, path.extname(targetPath)))
    : path.join(
      buildOutputDirectory,
      `${path.basename(targetPath, path.extname(targetPath))}.dll`
    );

  fs.mkdirSync(path.dirname(outputArg), { recursive: true });

  const dotnetArgs = [
    'run',
    ...(targetFramework ? ['--framework', targetFramework] : []),
    '--project',
    compilerProjectPath,
    '--property',
    'WarningLevel=0',
    '--',
    targetPath,
    '-o',
    outputArg,
    ...(targetFramework ? ['--framework', targetFramework] : [])
  ];

  output.appendLine(`Building Raven target: dotnet ${dotnetArgs.join(' ')}`);

  try {
    const { stdout, stderr } = await execFileAsync('dotnet', dotnetArgs, {
      cwd: workspaceFolder,
      maxBuffer: 10 * 1024 * 1024
    });

    if (stdout.trim().length > 0) {
      output.appendLine(stdout);
    }
    if (stderr.trim().length > 0) {
      output.appendLine(stderr);
    }
  } catch (error) {
    const e = error as Error & { stdout?: string; stderr?: string };
    if (e.stdout) output.appendLine(e.stdout);
    if (e.stderr) output.appendLine(e.stderr);
    throw new Error(`Raven build failed. See the Raven output channel for details. ${e.message}`);
  }

  const outputPath = targetIsProject
    ? path.join(outputArg, `${getProjectAssemblyName(targetPath)}.dll`)
    : outputArg;

  return { outputPath, cwd: path.dirname(targetPath) };
}

function resolveCommandTarget(uri?: vscode.Uri): string | undefined {
  const directTarget = uri?.scheme === 'file' ? uri.fsPath : undefined;
  if (directTarget && isRavenFile(directTarget)) {
    return directTarget;
  }

  const activeTarget = vscode.window.activeTextEditor?.document.fileName;
  if (activeTarget && isRavenFile(activeTarget)) {
    return activeTarget;
  }

  return undefined;
}

class RavenDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
  provideDebugConfigurations(): vscode.ProviderResult<vscode.DebugConfiguration[]> {
    return [{
      type: 'raven',
      request: 'launch',
      name: 'Raven: Compile and Debug',
      target: '${file}'
    }];
  }

  async resolveDebugConfiguration(
    _folder: vscode.WorkspaceFolder | undefined,
    config: vscode.DebugConfiguration
  ): Promise<vscode.DebugConfiguration | null | undefined> {
    const targetPath = resolveDebugTarget(config);
    if (!targetPath || !isRavenFile(targetPath)) {
      void vscode.window.showErrorMessage(
        'Select a .rav or .ravenproj file, or set "target"/"project" in launch.json.'
      );
      return undefined;
    }

    return vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: `Compiling ${path.basename(targetPath)}`
      },
      async () => {
        const { outputDllPath, cwd } = await compileForDebug(targetPath);
        return {
          name: config.name ?? 'Raven: Compile and Debug',
          type: 'coreclr',
          request: 'launch',
          program: 'dotnet',
          args: [outputDllPath],
          cwd,
          console: 'integratedTerminal',
          stopAtEntry: false
        };
      }
    );
  }
}

export function activate(context: vscode.ExtensionContext): void {
  output.appendLine('Activating Raven VS Code extension...');

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

  const debugConfigurationProvider = new RavenDebugConfigurationProvider();
  context.subscriptions.push(
    vscode.debug.registerDebugConfigurationProvider(
      'raven',
      debugConfigurationProvider,
      vscode.DebugConfigurationProviderTriggerKind.Dynamic
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.debug.compileAndDebug', async (uri?: vscode.Uri) => {
      const target = resolveCommandTarget(uri);
      if (!target) {
        void vscode.window.showErrorMessage('No active Raven file to debug.');
        return;
      }

      await vscode.debug.startDebugging(undefined, {
        type: 'raven',
        request: 'launch',
        name: 'Raven: Compile and Debug',
        target
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.build.activeTarget', async (uri?: vscode.Uri) => {
      const target = resolveCommandTarget(uri);
      if (!target) {
        void vscode.window.showErrorMessage('No active Raven file or project to build.');
        return;
      }

      await vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: `Building ${path.basename(target)}`
        },
        async () => {
          const { outputPath } = await buildTarget(target);
          output.appendLine(`Build output: ${outputPath}`);
          void vscode.window.showInformationMessage(`Raven build succeeded: ${path.basename(outputPath)}`);
        }
      );
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.build.clean', async (_uri?: vscode.Uri) => {
      const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
      const baseDirectory = workspaceFolder ?? process.cwd();
      const buildDirectory = path.join(baseDirectory, '.raven-build');
      const debugDirectory = path.join(baseDirectory, '.raven-debug');
      const removedPaths: string[] = [];

      for (const candidate of [buildDirectory, debugDirectory]) {
        if (!fs.existsSync(candidate)) {
          continue;
        }

        fs.rmSync(candidate, { recursive: true, force: true });
        removedPaths.push(candidate);
      }

      if (removedPaths.length === 0) {
        void vscode.window.showInformationMessage('Raven clean completed. No build artifacts were found.');
        return;
      }

      output.appendLine(`Cleaned Raven artifacts:\n- ${removedPaths.join('\n- ')}`);
      void vscode.window.showInformationMessage('Raven clean completed.');
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
