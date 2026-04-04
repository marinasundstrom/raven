import * as fs from 'fs';
import * as path from 'path';
import * as crypto from 'crypto';
import { execFile } from 'child_process';
import { promisify } from 'util';
import * as vscode from 'vscode';
import { CloseAction, ErrorAction, LanguageClient, LanguageClientOptions, ServerOptions, State, StateChangeEvent, Trace } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let clientStopPromise: Promise<void> | undefined;
let clientStartPromise: Promise<void> | undefined;
const execFileAsync = promisify(execFile);
const output = vscode.window.createOutputChannel('Raven');
let extensionInstallPath = '';

function appendLifecycleLog(message: string): void {
  output.appendLine(`[lifecycle ${new Date().toISOString()}] ${message}`);
}

function formatClientState(state: State): string {
  switch (state) {
    case State.Starting:
      return 'Starting';
    case State.Running:
      return 'Running';
    case State.Stopped:
      return 'Stopped';
    default:
      return `Unknown(${state})`;
  }
}

function logStateChange(event: StateChangeEvent): void {
  appendLifecycleLog(`Language client state changed: ${formatClientState(event.oldState)} -> ${formatClientState(event.newState)}`);
}

function formatRequestType(type: string | { method?: string }): string {
  if (typeof type === 'string') {
    return type;
  }

  if (type && typeof type === 'object' && typeof type.method === 'string') {
    return type.method;
  }

  return '<unknown>';
}

async function stopClient(reason: string): Promise<void> {
  const activeClient = client;
  if (!activeClient) {
    appendLifecycleLog(`stopClient(${reason}) skipped: no active client.`);
    return;
  }

  if (clientStopPromise) {
    appendLifecycleLog(`stopClient(${reason}) joined existing stop operation.`);
    return clientStopPromise;
  }

  const startedAt = Date.now();
  appendLifecycleLog(`stopClient(${reason}) started.`);

  clientStopPromise = activeClient.stop().then(
    () => {
      appendLifecycleLog(`stopClient(${reason}) completed in ${Date.now() - startedAt}ms.`);
    },
    error => {
      const message = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
      appendLifecycleLog(`stopClient(${reason}) failed after ${Date.now() - startedAt}ms: ${message}`);
      throw error;
    }
  ).finally(() => {
    if (client === activeClient) {
      client = undefined;
    }

    clientStopPromise = undefined;
  });

  return clientStopPromise;
}

function createLanguageClient(context: vscode.ExtensionContext): LanguageClient {
  let serverPath: string;
  try {
    serverPath = resolveServerPath(context, output);
  } catch (e) {
    const message = e instanceof Error ? e.message : String(e);
    output.appendLine(message);
    output.show(true);
    throw new Error(`Raven: ${message}`);
  }

  let isolatedServerPath: string;
  try {
    isolatedServerPath = stageServerForIsolatedLaunch(context, serverPath);
  } catch (e) {
    const message = e instanceof Error ? e.message : String(e);
    output.appendLine(`Failed to stage isolated language server: ${message}`);
    output.show(true);
    throw new Error(`Raven: Failed to stage isolated language server: ${message}`);
  }

  output.appendLine(`Using language server: ${serverPath}`);
  output.appendLine(`Using isolated language server: ${isolatedServerPath}`);

  const runCommand = {
    command: 'dotnet',
    args: [isolatedServerPath],
    options: {
      cwd: path.dirname(isolatedServerPath)
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
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{rvn,rav,rvnproj,csproj,fsproj}')
    },
    outputChannel: output,
    traceOutputChannel: output,
    errorHandler: {
      error(error, message, count) {
        const errorMessage = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
        const messageSummary = message ? JSON.stringify(message) : '<none>';
        appendLifecycleLog(
          `Language client transport error: error=${errorMessage} message=${messageSummary} count=${count ?? '<none>'}`
        );
        return { action: ErrorAction.Continue };
      },
      closed() {
        appendLifecycleLog('Language client transport closed.');
        return { action: CloseAction.DoNotRestart };
      }
    },
    middleware: {
      async sendRequest(type, param, token, next) {
        const method = formatRequestType(type);
        const interesting =
          method === 'textDocument/hover' ||
          method === 'textDocument/semanticTokens/full' ||
          method === 'textDocument/semanticTokens/range' ||
          method === 'textDocument/documentSymbol' ||
          method === 'textDocument/documentDiagnostic' ||
          method === 'workspace/diagnostic';

        const startedAt = Date.now();
        if (interesting) {
          appendLifecycleLog(`Request started: ${method}`);
        }

        try {
          const result = await next(type, param, token);
          if (interesting) {
            appendLifecycleLog(`Request completed: ${method} in ${Date.now() - startedAt}ms.`);
          }

          return result;
        } catch (error) {
          const message = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
          if (interesting) {
            appendLifecycleLog(`Request failed: ${method} after ${Date.now() - startedAt}ms: ${message}`);
          }

          throw error;
        }
      },
      async sendNotification(type, next, params) {
        const method = formatRequestType(type);
        const interesting =
          method === 'textDocument/didOpen' ||
          method === 'textDocument/didChange' ||
          method === 'textDocument/didSave' ||
          method === 'textDocument/didClose';

        if (interesting) {
          appendLifecycleLog(`Notification sent: ${method}`);
        }

        return next(type, params);
      }
    }
  };

  const createdClient = new LanguageClient(
    'ravenLanguageServer',
    'Raven Language Server',
    serverOptions,
    clientOptions
  );
  createdClient.onDidChangeState(logStateChange);
  createdClient.setTrace(Trace.Verbose);
  appendLifecycleLog('Language client trace level set to Verbose.');
  return createdClient;
}

async function startClient(context: vscode.ExtensionContext, reason: string): Promise<void> {
  if (clientStartPromise) {
    appendLifecycleLog(`startClient(${reason}) joined existing start operation.`);
    return clientStartPromise;
  }

  clientStartPromise = (async () => {
    if (client) {
      appendLifecycleLog(`startClient(${reason}) skipped: client already active.`);
      return;
    }

    try {
      client = createLanguageClient(context);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      void vscode.window.showErrorMessage(message);
      throw error;
    }

    appendLifecycleLog(`Starting language client (${reason}).`);
    await client.start();
  })().finally(() => {
    clientStartPromise = undefined;
  });

  return clientStartPromise;
}

async function restartClient(context: vscode.ExtensionContext, reason: string): Promise<void> {
  appendLifecycleLog(`restartClient(${reason}) requested.`);
  await stopClient(`restart:${reason}`);
  await startClient(context, `restart:${reason}`);
}

class RavenDocumentationContentProvider implements vscode.TextDocumentContentProvider {
  provideTextDocumentContent(uri: vscode.Uri): string {
    const params = new URLSearchParams(uri.query);
    const label = params.get('label')?.trim() || 'Documentation';
    const target = params.get('target')?.trim() || '';

    const lines = [
      `# ${label}`,
      '',
      'This editor link targets the following symbol reference:',
      '',
      target.length > 0 ? `- Symbol: \`${target}\`` : '- Symbol: unavailable',
      '',
      'Full symbol-page resolution will be provided by the dedicated Raven documentation view.'
    ];

    return `${lines.join('\n')}\n`;
  }
}

function parseDocumentationUriArgument(value: unknown): vscode.Uri | undefined {
  if (value instanceof vscode.Uri) {
    return value;
  }

  if (typeof value === 'string' && value.trim().length > 0) {
    try {
      return vscode.Uri.parse(value);
    } catch {
      return undefined;
    }
  }

  if (value && typeof value === 'object' && 'scheme' in value) {
    try {
      return vscode.Uri.from(value as { scheme: string; authority?: string; path?: string; query?: string; fragment?: string });
    } catch {
      return undefined;
    }
  }

  return undefined;
}

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

  // 1) Dev/workspace copy next to the extension folder.
  // Prefer this over the packaged server so local compiler/language-server
  // changes are reflected in diagnostics during development.
  // <repo>/src/Raven.LanguageServer/bin/{Debug|Release}/{tfm}/Raven.LanguageServer.dll
  const repoCandidateRoots = [
    path.join(context.extensionPath, '..', 'Raven.LanguageServer', 'bin'),
    path.join(context.extensionPath, '..', '..', 'Raven.LanguageServer', 'bin')
  ];

  const configurations = ['Debug', 'Release'];
  const tfms = ['net10.0', 'net8.0', 'net7.0'];

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

  // 2) Packaged copy: <extension>/server/Raven.LanguageServer.dll
  const packagedPath = context.asAbsolutePath(path.join('server', 'Raven.LanguageServer.dll'));
  attempts.push(packagedPath);
  if (fs.existsSync(packagedPath)) {
    return packagedPath;
  }

  output.appendLine('Failed to locate Raven.LanguageServer.dll. Tried:');
  for (const p of attempts) output.appendLine(`- ${p}`);

  throw new Error(
    'Unable to locate Raven.LanguageServer.dll. Build the language server or set "raven.languageServerPath" to the compiled DLL.'
  );
}

function createStableHash(input: string): string {
  return crypto.createHash('sha256').update(input).digest('hex').slice(0, 12);
}

function stageServerForIsolatedLaunch(context: vscode.ExtensionContext, sourceServerPath: string): string {
  const sourceDirectory = path.dirname(sourceServerPath);
  const serverStat = fs.statSync(sourceServerPath);
  const fingerprint = createStableHash(`${sourceServerPath}|${serverStat.mtimeMs}|${serverStat.size}`);
  const stagingRoot = path.join(context.globalStorageUri.fsPath, 'language-server');
  const targetDirectory = path.join(stagingRoot, fingerprint);
  const targetServerPath = path.join(targetDirectory, path.basename(sourceServerPath));

  fs.mkdirSync(stagingRoot, { recursive: true });

  if (!fs.existsSync(targetServerPath)) {
    fs.rmSync(targetDirectory, { recursive: true, force: true });
    fs.mkdirSync(targetDirectory, { recursive: true });
    fs.cpSync(sourceDirectory, targetDirectory, { recursive: true });
  }

  return targetServerPath;
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

  const searchRoots = new Set<string>();
  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    searchRoots.add(folder.uri.fsPath);
  }
  if (extensionInstallPath.length > 0) {
    searchRoots.add(extensionInstallPath);
  }
  searchRoots.add(process.cwd());

  for (const root of searchRoots) {
    for (const dir of enumerateAncestorDirectories(root)) {
      const candidate = path.join(dir, 'src', 'Raven.Compiler', 'Raven.Compiler.csproj');
      if (fs.existsSync(candidate)) {
        return candidate;
      }
    }
  }

  return undefined;
}

type CompilerInvocation = {
  executable: string;
  args: string[];
  description: string;
};

function resolveCompilerInvocation(targetFramework: string | undefined): CompilerInvocation | undefined {
  const bundledRoots = extensionInstallPath.length > 0
    ? [
        path.join(extensionInstallPath, 'compiler'),
        path.join(extensionInstallPath, 'server')
      ]
    : [];
  const preferredTfms = targetFramework
    ? [targetFramework, 'net10.0', 'net11.0']
    : ['net10.0', 'net11.0'];

  for (const root of bundledRoots) {
    for (const tfm of preferredTfms) {
      const tfmCandidate = path.join(root, tfm, 'rvn.dll');
      if (fs.existsSync(tfmCandidate)) {
        return {
          executable: 'dotnet',
          args: [tfmCandidate],
          description: tfmCandidate
        };
      }
    }

    const flatCandidate = path.join(root, 'rvn.dll');
    if (fs.existsSync(flatCandidate)) {
      return {
        executable: 'dotnet',
        args: [flatCandidate],
        description: flatCandidate
      };
    }
  }

  const compilerProjectPath = resolveCompilerProjectPath();
  if (!compilerProjectPath) {
    return undefined;
  }

  const compilerDirectory = path.dirname(compilerProjectPath);

  for (const tfm of preferredTfms) {
    const candidate = path.join(compilerDirectory, 'bin', 'Debug', tfm, 'rvn.dll');
    if (fs.existsSync(candidate)) {
      return {
        executable: 'dotnet',
        args: [candidate],
        description: candidate
      };
    }
  }

  return undefined;
}

function resolveTargetFramework(targetPath: string): string | undefined {
  const configuration = vscode.workspace.getConfiguration('raven');
  const configuredFramework = configuration.get<string>('targetFramework')?.trim();
  if (configuredFramework && configuredFramework.length > 0) {
    return configuredFramework;
  }

  const projectFilePath = resolveOwningProjectPath(targetPath);
  if (!projectFilePath) {
    return undefined;
  }

  return getProjectTargetFramework(projectFilePath);
}

function isRavenFile(filePath: string): boolean {
  const ext = path.extname(filePath).toLowerCase();
  return ext === '.rvn' || ext === '.rav' || ext === '.ravenproj' || ext === '.rvnproj';
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

function getProjectTargetFramework(projectFilePath: string): string | undefined {
  try {
    const xml = fs.readFileSync(projectFilePath, 'utf8');
    const attributeMatch = xml.match(/\bTargetFramework\s*=\s*"([^"]+)"/i);
    if (attributeMatch?.[1]?.trim()) {
      return attributeMatch[1].trim();
    }

    const elementMatch = xml.match(/<TargetFramework>\s*([^<]+)\s*<\/TargetFramework>/i);
    if (elementMatch?.[1]?.trim()) {
      return elementMatch[1].trim();
    }
  } catch {
    // Ignore read/parse issues and fall back to defaults.
  }

  return undefined;
}

function resolveOwningProjectPath(targetPath: string): string | undefined {
  if (isRavenProjectFile(targetPath)) {
    return fs.existsSync(targetPath) ? targetPath : undefined;
  }

  const workspaceBoundary = resolveWorkspaceBoundary(targetPath);
  for (const directory of enumerateAncestorDirectories(path.dirname(targetPath), workspaceBoundary)) {
    const candidates = findRavenProjectsInDirectory(directory);
    if (candidates.length === 0) {
      continue;
    }

    if (candidates.length === 1) {
      return candidates[0];
    }

    const directoryName = path.basename(directory).toLowerCase();
    const preferred = candidates.find(candidate =>
      path.basename(candidate, path.extname(candidate)).toLowerCase() === directoryName
    );
    return preferred ?? candidates[0];
  }

  return undefined;
}

function findRavenProjectsInDirectory(directory: string): string[] {
  try {
    return fs
      .readdirSync(directory)
      .filter(entry => isRavenProjectFile(entry))
      .map(entry => path.join(directory, entry))
      .sort((left, right) => left.localeCompare(right));
  } catch {
    return [];
  }
}

function isRavenProjectFile(filePath: string): boolean {
  const ext = path.extname(filePath).toLowerCase();
  return ext === '.ravenproj' || ext === '.rvnproj';
}

function* enumerateAncestorDirectories(startPath: string, stopDirectory?: string): Generator<string> {
  let current = path.resolve(startPath);
  const stop = stopDirectory ? path.resolve(stopDirectory) : undefined;
  while (true) {
    yield current;
    if (stop && current.toLowerCase() === stop.toLowerCase()) {
      break;
    }

    const parent = path.dirname(current);
    if (parent === current) {
      break;
    }

    current = parent;
  }
}

function resolveWorkspaceBoundary(targetPath: string): string | undefined {
  const targetDirectory = path.dirname(path.resolve(targetPath));
  const containingWorkspace = vscode.workspace.workspaceFolders
    ?.map(folder => folder.uri.fsPath)
    .find(folderPath => isWithinDirectory(targetDirectory, folderPath));

  return containingWorkspace;
}

function isWithinDirectory(candidatePath: string, directoryPath: string): boolean {
  const normalizedCandidate = path.resolve(candidatePath);
  const normalizedDirectory = path.resolve(directoryPath);

  if (normalizedCandidate.toLowerCase() === normalizedDirectory.toLowerCase()) {
    return true;
  }

  const prefix = normalizedDirectory.endsWith(path.sep)
    ? normalizedDirectory
    : `${normalizedDirectory}${path.sep}`;
  return normalizedCandidate.toLowerCase().startsWith(prefix.toLowerCase());
}

type OutputLayout = {
  effectiveTargetPath: string;
  targetIsProject: boolean;
  outputDirectory: string;
  outputDllPath: string;
  workspaceFolder: string;
  cwd: string;
  targetFramework?: string;
};

function getContainingWorkspaceFolderPath(targetPath: string): string {
  const targetDirectory = path.dirname(path.resolve(targetPath));
  const containingWorkspace = vscode.workspace.workspaceFolders
    ?.map(folder => folder.uri.fsPath)
    .find(folderPath => isWithinDirectory(targetDirectory, folderPath));
  return containingWorkspace ?? vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? targetDirectory;
}

function hashPathForOutput(targetPath: string): string {
  return crypto.createHash('sha256').update(path.resolve(targetPath)).digest('hex').slice(0, 12);
}

function normalizePathSegment(value: string): string {
  const normalized = value.trim().replace(/[^A-Za-z0-9_-]/g, '_');
  return normalized.length > 0 ? normalized : 'unknown';
}

function resolveOutputLayout(targetPath: string, configuration: 'Debug' | 'Release'): OutputLayout {
  const effectiveTargetPath = resolveEffectiveTargetPath(targetPath);
  const targetFramework = resolveTargetFramework(effectiveTargetPath);
  const targetIsProject = isRavenProjectFile(effectiveTargetPath);
  const workspaceFolder = getContainingWorkspaceFolderPath(effectiveTargetPath);

  if (targetIsProject) {
    const projectDirectory = path.dirname(effectiveTargetPath);
    const tfmSegment = normalizePathSegment(targetFramework ?? 'unknown-tfm');
    const outputDirectory = path.join(projectDirectory, 'bin', configuration, tfmSegment);
    return {
      effectiveTargetPath,
      targetIsProject,
      outputDirectory,
      outputDllPath: path.join(outputDirectory, `${getProjectAssemblyName(effectiveTargetPath)}.dll`),
      workspaceFolder,
      cwd: projectDirectory,
      targetFramework
    };
  }

  const fileBaseName = path.basename(effectiveTargetPath, path.extname(effectiveTargetPath));
  const tfmSegment = normalizePathSegment(targetFramework ?? 'no-tfm');
  const deterministicDirectory = `${fileBaseName}-${hashPathForOutput(effectiveTargetPath)}`;
  const outputDirectory = path.join(workspaceFolder, '.raven-build', configuration, tfmSegment, deterministicDirectory);
  return {
    effectiveTargetPath,
    targetIsProject,
    outputDirectory,
    outputDllPath: path.join(outputDirectory, `${fileBaseName}.dll`),
    workspaceFolder,
    cwd: path.dirname(effectiveTargetPath),
    targetFramework
  };
}

function writeBuildManifest(layout: OutputLayout, mode: 'build' | 'debug'): void {
  const manifestPath = path.join(layout.outputDirectory, '.raven-build-manifest.json');
  const manifest = {
    mode,
    targetPath: layout.effectiveTargetPath,
    targetKind: layout.targetIsProject ? 'project' : 'file',
    targetFramework: layout.targetFramework ?? null,
    outputDirectory: layout.outputDirectory,
    outputDllPath: layout.outputDllPath,
    cwd: layout.cwd,
    generatedAtUtc: new Date().toISOString()
  };
  fs.writeFileSync(manifestPath, JSON.stringify(manifest, null, 2));
}

async function compileForDebug(targetPath: string): Promise<{ outputDllPath: string; cwd: string }> {
  const layout = resolveOutputLayout(targetPath, 'Debug');
  const compilerInvocation = resolveCompilerInvocation(layout.targetFramework);
  if (!compilerInvocation) {
    throw new Error(
      'Unable to locate a built rvn.dll. Build Raven.Compiler first or point "raven.compilerProjectPath" at a workspace containing src/Raven.Compiler/bin/Debug/<tfm>/rvn.dll.'
    );
  }

  if (fs.existsSync(layout.outputDirectory)) {
    fs.rmSync(layout.outputDirectory, { recursive: true, force: true });
  }
  fs.mkdirSync(layout.outputDirectory, { recursive: true });

  const dotnetArgs = [
    ...compilerInvocation.args,
    layout.effectiveTargetPath,
    '--publish',
    '-o',
    layout.outputDirectory,
    ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
  ];

  output.appendLine(`Compiling for debug via ${compilerInvocation.description}: ${compilerInvocation.executable} ${dotnetArgs.join(' ')}`);

  try {
    const { stdout, stderr } = await execFileAsync(compilerInvocation.executable, dotnetArgs, {
      cwd: layout.workspaceFolder,
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

  if (!fs.existsSync(layout.outputDllPath)) {
    throw new Error(`Compiled assembly not found at '${layout.outputDllPath}'.`);
  }

  writeBuildManifest(layout, 'debug');
  return { outputDllPath: layout.outputDllPath, cwd: layout.cwd };
}

async function buildTarget(targetPath: string): Promise<{ outputPath: string; cwd: string }> {
  const layout = resolveOutputLayout(targetPath, 'Debug');
  const compilerInvocation = resolveCompilerInvocation(layout.targetFramework);
  if (!compilerInvocation) {
    throw new Error(
      'Unable to locate a built rvn.dll. Build Raven.Compiler first or point "raven.compilerProjectPath" at a workspace containing src/Raven.Compiler/bin/Debug/<tfm>/rvn.dll.'
    );
  }

  fs.mkdirSync(layout.outputDirectory, { recursive: true });
  const outputArg = layout.targetIsProject ? layout.outputDirectory : layout.outputDllPath;
  const publishArgs = layout.targetIsProject ? ['--publish'] : [];

  const dotnetArgs = [
    ...compilerInvocation.args,
    layout.effectiveTargetPath,
    ...publishArgs,
    '-o',
    outputArg,
    ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
  ];

  output.appendLine(`Building Raven target via ${compilerInvocation.description}: ${compilerInvocation.executable} ${dotnetArgs.join(' ')}`);

  try {
    const { stdout, stderr } = await execFileAsync(compilerInvocation.executable, dotnetArgs, {
      cwd: layout.workspaceFolder,
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

  writeBuildManifest(layout, 'build');
  return { outputPath: layout.outputDllPath, cwd: layout.cwd };
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

function resolveEffectiveTargetPath(targetPath: string): string {
  return resolveOwningProjectPath(targetPath) ?? targetPath;
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
        'Select a .rvn, .rvnproj, .rav, or .ravenproj file, or set "target"/"project" in launch.json.'
      );
      return undefined;
    }

    return vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: `Compiling ${path.basename(resolveEffectiveTargetPath(targetPath))}`
      },
      async () => {
        const { outputDllPath, cwd } = await compileForDebug(targetPath);
        const ravenConfiguration = vscode.workspace.getConfiguration('raven');
        const stopAtEntry = ravenConfiguration.get<boolean>('debugStopAtEntry', false);
        const justMyCode = ravenConfiguration.get<boolean>('debugJustMyCode', false);
        const moduleLoadMessages = ravenConfiguration.get<boolean>('debugModuleLoadMessages', false);
        const engineLogging = ravenConfiguration.get<boolean>('debugEngineLogging', false);
        const excludeFrameworkModules = ravenConfiguration.get<boolean>('debugExcludeFrameworkModules', true);

        const symbolOptions: {
          searchMicrosoftSymbolServer: boolean;
          searchNuGetOrgSymbolServer: boolean;
          moduleFilter?: {
            mode: string;
            excludedModules: string[];
          };
        } = {
          searchMicrosoftSymbolServer: false,
          searchNuGetOrgSymbolServer: false
        };

        if (excludeFrameworkModules) {
          symbolOptions.moduleFilter = {
            mode: 'loadAllButExcluded',
            excludedModules: [
              'System.*',
              'Microsoft.*'
            ]
          };
        }

        return {
          name: config.name ?? 'Raven: Compile and Debug',
          type: 'coreclr',
          request: 'launch',
          program: 'dotnet',
          args: [outputDllPath],
          cwd,
          console: 'integratedTerminal',
          stopAtEntry,
          justMyCode,
          requireExactSource: false,
          logging: {
            moduleLoad: moduleLoadMessages,
            engineLogging
          },
          symbolOptions
        };
      }
    );
  }
}

export function activate(context: vscode.ExtensionContext): void {
  extensionInstallPath = context.extensionPath;
  output.appendLine('Activating Raven VS Code extension...');
  appendLifecycleLog(`Extension activate() called. extensionPath=${context.extensionPath}`);

  // Ensure VS Code disposes the client on shutdown.
  context.subscriptions.push({
    dispose: () => {
      appendLifecycleLog('Extension subscription dispose() called.');
      return stopClient('subscription dispose');
    }
  });

  void startClient(context, 'activate');

  context.subscriptions.push(
    vscode.workspace.onDidChangeWorkspaceFolders(event => {
      const added = event.added.map(folder => folder.uri.fsPath).join(', ') || '<none>';
      const removed = event.removed.map(folder => folder.uri.fsPath).join(', ') || '<none>';
      appendLifecycleLog(`Workspace folders changed. added=${added} removed=${removed}`);
      void restartClient(context, 'workspace folders changed');
    })
  );

  const debugConfigurationProvider = new RavenDebugConfigurationProvider();
  context.subscriptions.push(
    vscode.debug.registerDebugConfigurationProvider(
      'raven',
      debugConfigurationProvider,
      vscode.DebugConfigurationProviderTriggerKind.Dynamic
    )
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.showMacroExpansion', async (_uri?: string, macroName?: string, expansionText?: string) => {
      if (!expansionText || expansionText.trim().length === 0) {
        void vscode.window.showInformationMessage('No macro expansion is available at the current location.');
        return;
      }

      const header = macroName && macroName.trim().length > 0
        ? `// Macro expansion for #[${macroName}]\n\n`
        : '// Macro expansion\n\n';

      const document = await vscode.workspace.openTextDocument({
        content: `${header}${expansionText}\n`,
        language: 'raven'
      });

      await vscode.window.showTextDocument(document, {
        preview: true,
        viewColumn: vscode.ViewColumn.Beside
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.showCodeActionPreview', async (_uri?: string, actionTitle?: string, beforeText?: string, afterText?: string) => {
      if (typeof beforeText !== 'string' || typeof afterText !== 'string') {
        void vscode.window.showInformationMessage('No code action preview is available.');
        return;
      }

      const beforeDocument = await vscode.workspace.openTextDocument({
        content: beforeText,
        language: 'raven'
      });

      const afterDocument = await vscode.workspace.openTextDocument({
        content: afterText,
        language: 'raven'
      });

      const title = actionTitle && actionTitle.trim().length > 0
        ? `Preview: ${actionTitle}`
        : 'Code Action Preview';

      await vscode.commands.executeCommand(
        'vscode.diff',
        beforeDocument.uri,
        afterDocument.uri,
        title,
        { preview: true }
      );
    })
  );

  const documentationProvider = new RavenDocumentationContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider('raven-doc', documentationProvider)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.openDocumentation', async (uriOrString?: unknown) => {
      const uri = parseDocumentationUriArgument(uriOrString);
      if (!uri) {
        void vscode.window.showErrorMessage('Unable to open Raven documentation for this symbol.');
        return;
      }

      const document = await vscode.workspace.openTextDocument(uri);
      await vscode.window.showTextDocument(document, {
        preview: true,
        viewColumn: vscode.ViewColumn.Beside
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.debug.compileAndDebug', async (uri?: vscode.Uri) => {
      const target = resolveCommandTarget(uri);
      if (!target) {
        void vscode.window.showErrorMessage('No active Raven file to debug.');
        return;
      }
      const effectiveTarget = resolveEffectiveTargetPath(target);

      await vscode.debug.startDebugging(undefined, {
        type: 'raven',
        request: 'launch',
        name: 'Raven: Compile and Debug',
        target: effectiveTarget
      });
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('raven.run.compileAndRun', async (uri?: vscode.Uri) => {
      const target = resolveCommandTarget(uri);
      if (!target) {
        void vscode.window.showErrorMessage('No active Raven file to run.');
        return;
      }

      await vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: `Compiling ${path.basename(resolveEffectiveTargetPath(target))}`
        },
        async () => {
          const { outputPath, cwd } = await buildTarget(target);
          const terminal = vscode.window.createTerminal({ name: 'Raven: Run', cwd });
          terminal.show(true);
          // Quote the path to handle spaces.
          terminal.sendText(`dotnet "${outputPath}"`);
        }
      );
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
      const removedPaths: string[] = [];
      const workspaceFolders = vscode.workspace.workspaceFolders?.map(folder => folder.uri.fsPath)
        ?? [process.cwd()];

      for (const workspaceFolder of workspaceFolders) {
        const candidate = path.join(workspaceFolder, '.raven-build');
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
  appendLifecycleLog('Extension deactivate() called.');
  return stopClient('deactivate');
}
