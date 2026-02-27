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
const child_process_1 = require("child_process");
const util_1 = require("util");
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
const execFileAsync = (0, util_1.promisify)(child_process_1.execFile);
const output = vscode.window.createOutputChannel('Raven');
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
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    if (!workspaceFolder) {
        return undefined;
    }
    const defaultPath = path.join(workspaceFolder, 'src', 'Raven.Compiler', 'Raven.Compiler.csproj');
    return fs.existsSync(defaultPath) ? defaultPath : undefined;
}
function resolveTargetFramework() {
    const configuration = vscode.workspace.getConfiguration('raven');
    const configuredFramework = configuration.get('targetFramework')?.trim();
    return configuredFramework && configuredFramework.length > 0 ? configuredFramework : undefined;
}
function isRavenFile(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    return ext === '.rav' || ext === '.ravenproj';
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
    const match = xml.match(/<AssemblyName>\s*([^<]+)\s*<\/AssemblyName>/i);
    return match?.[1]?.trim() || fallback;
}
async function compileForDebug(targetPath) {
    const compilerProjectPath = resolveCompilerProjectPath();
    if (!compilerProjectPath) {
        throw new Error('Unable to locate Raven.Compiler.csproj. Set "raven.compilerProjectPath" to continue.');
    }
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? path.dirname(targetPath);
    const debugOutputDirectory = path.join(workspaceFolder, '.raven-debug');
    fs.mkdirSync(debugOutputDirectory, { recursive: true });
    const targetFramework = resolveTargetFramework();
    const targetIsProject = path.extname(targetPath).toLowerCase() === '.ravenproj';
    const outputArg = targetIsProject
        ? debugOutputDirectory
        : path.join(debugOutputDirectory, `${path.basename(targetPath, path.extname(targetPath))}.dll`);
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
    }
    catch (error) {
        const e = error;
        if (e.stdout)
            output.appendLine(e.stdout);
        if (e.stderr)
            output.appendLine(e.stderr);
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
            void vscode.window.showErrorMessage('Select a .rav or .ravenproj file, or set "target"/"project" in launch.json.');
            return undefined;
        }
        return vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            title: `Compiling ${path.basename(targetPath)}`
        }, async () => {
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
        });
    }
}
function activate(context) {
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
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.rav')
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
    context.subscriptions.push(vscode.commands.registerCommand('raven.debug.compileAndDebug', async (uri) => {
        const target = uri?.scheme === 'file' ? uri.fsPath : vscode.window.activeTextEditor?.document.fileName;
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
    }));
}
function deactivate() {
    return client?.stop();
}
