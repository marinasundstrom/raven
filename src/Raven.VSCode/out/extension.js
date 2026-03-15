"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
const crypto = __importStar(require("crypto"));
const child_process_1 = require("child_process");
const util_1 = require("util");
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
const execFileAsync = (0, util_1.promisify)(child_process_1.execFile);
const output = vscode.window.createOutputChannel('Raven');
let extensionInstallPath = '';
class RavenDocumentationContentProvider {
    provideTextDocumentContent(uri) {
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
function parseDocumentationUriArgument(value) {
    if (value instanceof vscode.Uri) {
        return value;
    }
    if (typeof value === 'string' && value.trim().length > 0) {
        try {
            return vscode.Uri.parse(value);
        }
        catch {
            return undefined;
        }
    }
    if (value && typeof value === 'object' && 'scheme' in value) {
        try {
            return vscode.Uri.from(value);
        }
        catch {
            return undefined;
        }
    }
    return undefined;
}
function resolveServerPath(context, output) {
    const configuration = vscode.workspace.getConfiguration('raven');
    const configuredPath = configuration.get('languageServerPath')?.trim();
    const attempts = [];
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
    for (const p of attempts)
        output.appendLine(`- ${p}`);
    throw new Error('Unable to locate Raven.LanguageServer.dll. Build the language server or set "raven.languageServerPath" to the compiled DLL.');
}
function resolveCompilerProjectPath() {
    const configuration = vscode.workspace.getConfiguration('raven');
    const configuredPath = configuration.get('compilerProjectPath')?.trim();
    if (configuredPath) {
        const absolutePath = path.isAbsolute(configuredPath)
            ? configuredPath
            : path.resolve(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? '', configuredPath);
        return fs.existsSync(absolutePath) ? absolutePath : undefined;
    }
    const searchRoots = new Set();
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
function resolveTargetFramework(targetPath) {
    const configuration = vscode.workspace.getConfiguration('raven');
    const configuredFramework = configuration.get('targetFramework')?.trim();
    if (configuredFramework && configuredFramework.length > 0) {
        return configuredFramework;
    }
    const projectFilePath = resolveOwningProjectPath(targetPath);
    if (!projectFilePath) {
        return undefined;
    }
    return getProjectTargetFramework(projectFilePath);
}
function isRavenFile(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    return ext === '.rvn' || ext === '.rav' || ext === '.ravenproj' || ext === '.rvnproj';
}
function resolveDebugTarget(config) {
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
function getProjectAssemblyName(projectFilePath) {
    const fallback = path.basename(projectFilePath, path.extname(projectFilePath));
    const xml = fs.readFileSync(projectFilePath, 'utf8');
    const assemblyNameMatch = xml.match(/<AssemblyName>\s*([^<]+)\s*<\/AssemblyName>/i);
    const outputAttributeMatch = xml.match(/\bOutput\s*=\s*"([^"]+)"/i);
    return assemblyNameMatch?.[1]?.trim() || outputAttributeMatch?.[1]?.trim() || fallback;
}
function getProjectTargetFramework(projectFilePath) {
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
    }
    catch {
        // Ignore read/parse issues and fall back to defaults.
    }
    return undefined;
}
function resolveOwningProjectPath(targetPath) {
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
        const preferred = candidates.find(candidate => path.basename(candidate, path.extname(candidate)).toLowerCase() === directoryName);
        return preferred ?? candidates[0];
    }
    return undefined;
}
function findRavenProjectsInDirectory(directory) {
    try {
        return fs
            .readdirSync(directory)
            .filter(entry => isRavenProjectFile(entry))
            .map(entry => path.join(directory, entry))
            .sort((left, right) => left.localeCompare(right));
    }
    catch {
        return [];
    }
}
function isRavenProjectFile(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    return ext === '.ravenproj' || ext === '.rvnproj';
}
function* enumerateAncestorDirectories(startPath, stopDirectory) {
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
function resolveWorkspaceBoundary(targetPath) {
    const targetDirectory = path.dirname(path.resolve(targetPath));
    const containingWorkspace = vscode.workspace.workspaceFolders
        ?.map(folder => folder.uri.fsPath)
        .find(folderPath => isWithinDirectory(targetDirectory, folderPath));
    return containingWorkspace;
}
function isWithinDirectory(candidatePath, directoryPath) {
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
function getContainingWorkspaceFolderPath(targetPath) {
    const targetDirectory = path.dirname(path.resolve(targetPath));
    const containingWorkspace = vscode.workspace.workspaceFolders
        ?.map(folder => folder.uri.fsPath)
        .find(folderPath => isWithinDirectory(targetDirectory, folderPath));
    return containingWorkspace ?? vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? targetDirectory;
}
function hashPathForOutput(targetPath) {
    return crypto.createHash('sha256').update(path.resolve(targetPath)).digest('hex').slice(0, 12);
}
function normalizePathSegment(value) {
    const normalized = value.trim().replace(/[^A-Za-z0-9_-]/g, '_');
    return normalized.length > 0 ? normalized : 'unknown';
}
function resolveOutputLayout(targetPath, configuration) {
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
function writeBuildManifest(layout, mode) {
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
async function compileForDebug(targetPath) {
    const compilerProjectPath = resolveCompilerProjectPath();
    if (!compilerProjectPath) {
        throw new Error('Unable to locate Raven.Compiler.csproj. Set "raven.compilerProjectPath" to continue.');
    }
    const layout = resolveOutputLayout(targetPath, 'Debug');
    if (fs.existsSync(layout.outputDirectory)) {
        fs.rmSync(layout.outputDirectory, { recursive: true, force: true });
    }
    fs.mkdirSync(layout.outputDirectory, { recursive: true });
    const dotnetArgs = [
        'run',
        ...(layout.targetFramework ? ['--framework', layout.targetFramework] : []),
        '--project',
        compilerProjectPath,
        '--property',
        'WarningLevel=0',
        '--',
        layout.effectiveTargetPath,
        '--publish',
        '-o',
        layout.outputDirectory,
        ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
    ];
    output.appendLine(`Compiling for debug: dotnet ${dotnetArgs.join(' ')}`);
    try {
        const { stdout, stderr } = await execFileAsync('dotnet', dotnetArgs, {
            cwd: layout.workspaceFolder,
            maxBuffer: 10 * 1024 * 1024
        });
        if (stdout.trim().length > 0) {
            output.appendLine(stdout);
        }
        if (stderr.trim().length > 0) {
            output.appendLine(stderr);
        }
    }
    catch (error) {
        const e = error;
        if (e.stdout)
            output.appendLine(e.stdout);
        if (e.stderr)
            output.appendLine(e.stderr);
        throw new Error(`Raven compile failed. See the Raven output channel for details. ${e.message}`);
    }
    if (!fs.existsSync(layout.outputDllPath)) {
        throw new Error(`Compiled assembly not found at '${layout.outputDllPath}'.`);
    }
    writeBuildManifest(layout, 'debug');
    return { outputDllPath: layout.outputDllPath, cwd: layout.cwd };
}
async function buildTarget(targetPath) {
    const compilerProjectPath = resolveCompilerProjectPath();
    if (!compilerProjectPath) {
        throw new Error('Unable to locate Raven.Compiler.csproj. Set "raven.compilerProjectPath" to continue.');
    }
    const layout = resolveOutputLayout(targetPath, 'Debug');
    fs.mkdirSync(layout.outputDirectory, { recursive: true });
    const outputArg = layout.targetIsProject ? layout.outputDirectory : layout.outputDllPath;
    const dotnetArgs = [
        'run',
        ...(layout.targetFramework ? ['--framework', layout.targetFramework] : []),
        '--project',
        compilerProjectPath,
        '--property',
        'WarningLevel=0',
        '--',
        layout.effectiveTargetPath,
        '-o',
        outputArg,
        ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
    ];
    output.appendLine(`Building Raven target: dotnet ${dotnetArgs.join(' ')}`);
    try {
        const { stdout, stderr } = await execFileAsync('dotnet', dotnetArgs, {
            cwd: layout.workspaceFolder,
            maxBuffer: 10 * 1024 * 1024
        });
        if (stdout.trim().length > 0) {
            output.appendLine(stdout);
        }
        if (stderr.trim().length > 0) {
            output.appendLine(stderr);
        }
    }
    catch (error) {
        const e = error;
        if (e.stdout)
            output.appendLine(e.stdout);
        if (e.stderr)
            output.appendLine(e.stderr);
        throw new Error(`Raven build failed. See the Raven output channel for details. ${e.message}`);
    }
    writeBuildManifest(layout, 'build');
    return { outputPath: layout.outputDllPath, cwd: layout.cwd };
}
function resolveCommandTarget(uri) {
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
function resolveEffectiveTargetPath(targetPath) {
    return resolveOwningProjectPath(targetPath) ?? targetPath;
}
class RavenDebugConfigurationProvider {
    provideDebugConfigurations() {
        return [{
                type: 'raven',
                request: 'launch',
                name: 'Raven: Compile and Debug',
                target: '${file}'
            }];
    }
    async resolveDebugConfiguration(_folder, config) {
        const targetPath = resolveDebugTarget(config);
        if (!targetPath || !isRavenFile(targetPath)) {
            void vscode.window.showErrorMessage('Select a .rvn, .rvnproj, .rav, or .ravenproj file, or set "target"/"project" in launch.json.');
            return undefined;
        }
        return vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            title: `Compiling ${path.basename(resolveEffectiveTargetPath(targetPath))}`
        }, async () => {
            const { outputDllPath, cwd } = await compileForDebug(targetPath);
            const ravenConfiguration = vscode.workspace.getConfiguration('raven');
            const stopAtEntry = ravenConfiguration.get('debugStopAtEntry', false);
            const justMyCode = ravenConfiguration.get('debugJustMyCode', false);
            const moduleLoadMessages = ravenConfiguration.get('debugModuleLoadMessages', false);
            const engineLogging = ravenConfiguration.get('debugEngineLogging', false);
            const excludeFrameworkModules = ravenConfiguration.get('debugExcludeFrameworkModules', true);
            const symbolOptions = {
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
        });
    }
}
function activate(context) {
    extensionInstallPath = context.extensionPath;
    output.appendLine('Activating Raven VS Code extension...');
    let serverPath;
    try {
        serverPath = resolveServerPath(context, output);
    }
    catch (e) {
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
    const serverOptions = {
        run: runCommand,
        debug: runCommand
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'raven' }],
        synchronize: {
            configurationSection: 'raven',
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{rvn,rav}')
        },
        outputChannel: output
    };
    client = new node_1.LanguageClient('ravenLanguageServer', 'Raven Language Server', serverOptions, clientOptions);
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
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('raven', debugConfigurationProvider, vscode.DebugConfigurationProviderTriggerKind.Dynamic));
    context.subscriptions.push(vscode.commands.registerCommand('raven.showMacroExpansion', async (_uri, macroName, expansionText) => {
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
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.showCodeActionPreview', async (_uri, actionTitle, beforeText, afterText) => {
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
        await vscode.commands.executeCommand('vscode.diff', beforeDocument.uri, afterDocument.uri, title, { preview: true });
    }));
    const documentationProvider = new RavenDocumentationContentProvider();
    context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider('raven-doc', documentationProvider));
    context.subscriptions.push(vscode.commands.registerCommand('raven.openDocumentation', async (uriOrString) => {
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
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.debug.compileAndDebug', async (uri) => {
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
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.run.compileAndRun', async (uri) => {
        const target = resolveCommandTarget(uri);
        if (!target) {
            void vscode.window.showErrorMessage('No active Raven file to run.');
            return;
        }
        await vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            title: `Compiling ${path.basename(resolveEffectiveTargetPath(target))}`
        }, async () => {
            const { outputPath, cwd } = await buildTarget(target);
            const terminal = vscode.window.createTerminal({ name: 'Raven: Run', cwd });
            terminal.show(true);
            // Quote the path to handle spaces.
            terminal.sendText(`dotnet "${outputPath}"`);
        });
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.build.activeTarget', async (uri) => {
        const target = resolveCommandTarget(uri);
        if (!target) {
            void vscode.window.showErrorMessage('No active Raven file or project to build.');
            return;
        }
        await vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            title: `Building ${path.basename(target)}`
        }, async () => {
            const { outputPath } = await buildTarget(target);
            output.appendLine(`Build output: ${outputPath}`);
            void vscode.window.showInformationMessage(`Raven build succeeded: ${path.basename(outputPath)}`);
        });
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.build.clean', async (_uri) => {
        const removedPaths = [];
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
    }));
}
function deactivate() {
    return client?.stop();
}
