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
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
let clientStopPromise;
let clientStartPromise;
let languageServerBuildPromise;
const output = vscode.window.createOutputChannel('Raven');
let extensionInstallPath = '';
let pendingInlayHintRefresh;
let pendingImportCompletionTrigger;
const recentRavenDocumentChanges = new Map();
const inlayHintRequestVersions = new Map();
let inlayHintRefreshEpoch = 0;
let inlayHintRefreshPromise;
function execFileText(command, args, options = {}) {
    return new Promise((resolve, reject) => {
        (0, child_process_1.execFile)(command, [...args], { ...options, encoding: 'buffer' }, (error, stdout, stderr) => {
            const result = {
                stdout: bufferToText(stdout),
                stderr: bufferToText(stderr)
            };
            if (error) {
                const execError = error;
                execError.stdout = result.stdout;
                execError.stderr = result.stderr;
                reject(execError);
                return;
            }
            resolve(result);
        });
    });
}
function bufferToText(value) {
    if (value === null || value === undefined) {
        return '';
    }
    return Buffer.isBuffer(value) || value instanceof Uint8Array
        ? Buffer.from(value).toString('utf8')
        : value;
}
function appendLifecycleLog(message) {
    output.appendLine(`[lifecycle ${new Date().toISOString()}] ${message}`);
}
function formatClientState(state) {
    switch (state) {
        case node_1.State.Starting:
            return 'Starting';
        case node_1.State.Running:
            return 'Running';
        case node_1.State.Stopped:
            return 'Stopped';
        default:
            return `Unknown(${state})`;
    }
}
function logStateChange(event) {
    appendLifecycleLog(`Language client state changed: ${formatClientState(event.oldState)} -> ${formatClientState(event.newState)}`);
}
function formatRequestType(type) {
    if (typeof type === 'string') {
        return type;
    }
    if (type && typeof type === 'object' && typeof type.method === 'string') {
        return type.method;
    }
    return '<unknown>';
}
function formatRequestTarget(param) {
    if (!param || typeof param !== 'object') {
        return '';
    }
    const candidate = param;
    const uri = candidate.textDocument?.uri ?? candidate.uri;
    if (!uri) {
        return '';
    }
    const position = candidate.position;
    if (position?.line !== undefined && position.character !== undefined) {
        return ` ${uri} ${position.line}:${position.character}`;
    }
    const range = candidate.range;
    if (range?.start?.line !== undefined &&
        range.start.character !== undefined &&
        range.end?.line !== undefined &&
        range.end.character !== undefined) {
        return ` ${uri} ${range.start.line}:${range.start.character}-${range.end.line}:${range.end.character}`;
    }
    return ` ${uri}`;
}
function formatRequestResult(method, result) {
    if (method !== 'textDocument/completion') {
        return '';
    }
    if (Array.isArray(result)) {
        return ` items=${result.length}`;
    }
    if (result && typeof result === 'object') {
        const candidate = result;
        if (Array.isArray(candidate.items)) {
            return ` items=${candidate.items.length}`;
        }
    }
    return '';
}
function areInferredTypeInlayHintsEnabled() {
    return vscode.workspace
        .getConfiguration('raven')
        .get('inlayHints.inferredTypes.enabled', true);
}
function areRavenInlayHintsEnabled() {
    return vscode.workspace
        .getConfiguration('raven')
        .get('inlayHints.enabled', true);
}
function areNameInlayHintsEnabled() {
    return vscode.workspace
        .getConfiguration('raven')
        .get('inlayHints.names.enabled', true);
}
function getInlayHintRequestDebounceMilliseconds() {
    const configured = vscode.workspace
        .getConfiguration('raven')
        .get('inlayHints.requestDebounceMilliseconds', 250);
    if (!Number.isFinite(configured)) {
        return 250;
    }
    return Math.max(0, Math.min(2000, Math.trunc(configured)));
}
function bumpInlayHintRequestVersion(key) {
    const requestVersion = (inlayHintRequestVersions.get(key) ?? 0) + 1;
    inlayHintRequestVersions.set(key, requestVersion);
    return requestVersion;
}
function isCurrentInlayHintRequest(key, requestVersion) {
    return inlayHintRequestVersions.get(key) === requestVersion;
}
function invalidateVisibleInlayHintRequests() {
    let invalidated = 0;
    for (const editor of vscode.window.visibleTextEditors) {
        if (editor.document.languageId !== 'raven') {
            continue;
        }
        bumpInlayHintRequestVersion(editor.document.uri.toString());
        invalidated++;
    }
    if (invalidated > 0) {
        appendLifecycleLog(`Invalidated in-flight inlay hint request(s) for ${invalidated} visible Raven document(s).`);
    }
    return invalidated;
}
function areSemanticTokensEnabled() {
    return vscode.workspace
        .getConfiguration('raven')
        .get('semanticTokens.enabled', true);
}
function isSemanticTokensRequest(method) {
    return method === 'textDocument/semanticTokens/full' ||
        method === 'textDocument/semanticTokens/range';
}
function delayUnlessCanceled(milliseconds, token) {
    if (milliseconds <= 0) {
        return Promise.resolve(!token.isCancellationRequested);
    }
    if (token.isCancellationRequested) {
        return Promise.resolve(false);
    }
    return new Promise(resolve => {
        let disposable;
        const timer = setTimeout(() => {
            disposable?.dispose();
            resolve(!token.isCancellationRequested);
        }, milliseconds);
        disposable = token.onCancellationRequested(() => {
            clearTimeout(timer);
            disposable?.dispose();
            resolve(false);
        });
    });
}
function delay(milliseconds) {
    if (milliseconds <= 0) {
        return Promise.resolve();
    }
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}
async function waitForInlayHintQuietPeriod(document, token) {
    const debounceMilliseconds = getInlayHintRequestDebounceMilliseconds();
    if (debounceMilliseconds <= 0) {
        return !token.isCancellationRequested;
    }
    const key = document.uri.toString();
    while (!token.isCancellationRequested) {
        const state = recentRavenDocumentChanges.get(key);
        if (!state) {
            return true;
        }
        if (state.version !== document.version) {
            return false;
        }
        const remaining = debounceMilliseconds - (Date.now() - state.changedAt);
        if (remaining <= 0) {
            return true;
        }
        const completed = await delayUnlessCanceled(remaining, token);
        if (!completed) {
            return false;
        }
    }
    return false;
}
async function refreshInlayHints() {
    inlayHintRefreshEpoch++;
    if (!inlayHintRefreshPromise) {
        inlayHintRefreshPromise = runInlayHintRefreshLoop()
            .finally(() => {
            inlayHintRefreshPromise = undefined;
        });
    }
    return inlayHintRefreshPromise;
}
async function runInlayHintRefreshLoop() {
    let completedEpoch = 0;
    do {
        const refreshEpoch = inlayHintRefreshEpoch;
        pulseVisibleInlayHintProviders(refreshEpoch, 'primary');
        await delay(125);
        if (refreshEpoch !== inlayHintRefreshEpoch) {
            appendLifecycleLog(`Inlay hint refresh epoch ${refreshEpoch} superseded before settled pulse.`);
            continue;
        }
        pulseVisibleInlayHintProviders(refreshEpoch, 'settled');
        completedEpoch = refreshEpoch;
    } while (completedEpoch !== inlayHintRefreshEpoch);
}
function pulseVisibleInlayHintProviders(refreshEpoch, phase) {
    const invalidatedDocumentCount = invalidateVisibleInlayHintRequests();
    const providerCount = fireVisibleInlayHintProviders();
    appendLifecycleLog(`Inlay hint refresh pulse completed: epoch=${refreshEpoch} phase=${phase} invalidatedDocuments=${invalidatedDocumentCount} providers=${providerCount}.`);
}
function fireVisibleInlayHintProviders() {
    const activeClient = client;
    if (!activeClient || activeClient.state !== node_1.State.Running) {
        return 0;
    }
    let providerCount = 0;
    const seen = new Set();
    try {
        const feature = activeClient.getFeature(node_1.InlayHintRequest.method);
        for (const editor of vscode.window.visibleTextEditors) {
            if (editor.document.languageId !== 'raven') {
                continue;
            }
            const provider = feature.getProvider(editor.document);
            if (!provider || seen.has(provider)) {
                continue;
            }
            seen.add(provider);
            provider.onDidChangeInlayHints.fire();
            providerCount++;
        }
    }
    catch (error) {
        const message = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
        appendLifecycleLog(`Unable to invalidate inlay hint providers: ${message}`);
        return 0;
    }
    if (providerCount > 0) {
        appendLifecycleLog(`Invalidated ${providerCount} visible inlay hint provider(s).`);
    }
    return providerCount;
}
function scheduleInlayHintRefresh() {
    if (pendingInlayHintRefresh) {
        clearTimeout(pendingInlayHintRefresh);
    }
    pendingInlayHintRefresh = setTimeout(() => {
        pendingInlayHintRefresh = undefined;
        void refreshInlayHints();
    }, 50);
}
function shouldTriggerImportCompletionAfterQuietPeriod(event) {
    if (event.contentChanges.length === 0) {
        return false;
    }
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.uri.toString() !== event.document.uri.toString()) {
        return false;
    }
    return event.contentChanges.some(change => {
        const lineNumber = Math.min(change.range.start.line, event.document.lineCount - 1);
        const line = event.document.lineAt(lineNumber).text;
        return /^\s*import\s+[\w.]*\.\s*$/.test(line);
    });
}
function scheduleImportCompletionTrigger(document) {
    if (pendingImportCompletionTrigger) {
        clearTimeout(pendingImportCompletionTrigger);
    }
    const uri = document.uri.toString();
    const version = document.version;
    pendingImportCompletionTrigger = setTimeout(() => {
        pendingImportCompletionTrigger = undefined;
        const editor = vscode.window.activeTextEditor;
        if (!editor ||
            editor.document.uri.toString() !== uri ||
            editor.document.version !== version ||
            editor.document.languageId !== 'raven') {
            return;
        }
        const cursor = editor.selection.active;
        const line = editor.document.lineAt(cursor.line).text;
        const prefix = line.slice(0, cursor.character);
        if (!/^\s*import\s+[\w.]*\.$/.test(prefix)) {
            return;
        }
        appendLifecycleLog(`Triggering import completion after document quiet period at ${uri} ${cursor.line}:${cursor.character}.`);
        void vscode.commands.executeCommand('editor.action.triggerSuggest');
    }, 125);
}
async function stopClient(reason) {
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
    clientStopPromise = activeClient.stop().then(() => {
        appendLifecycleLog(`stopClient(${reason}) completed in ${Date.now() - startedAt}ms.`);
    }, error => {
        const message = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
        appendLifecycleLog(`stopClient(${reason}) failed after ${Date.now() - startedAt}ms: ${message}`);
        throw error;
    }).finally(() => {
        if (client === activeClient) {
            client = undefined;
        }
        clientStopPromise = undefined;
    });
    return clientStopPromise;
}
function resolveLanguageServerProjectPath() {
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
            const candidate = path.join(dir, 'src', 'Raven.LanguageServer', 'Raven.LanguageServer.csproj');
            if (fs.existsSync(candidate)) {
                return candidate;
            }
        }
    }
    return undefined;
}
async function ensureLanguageServerBuilt() {
    const configuration = vscode.workspace.getConfiguration('raven');
    if (!configuration.get('autoBuildLanguageServerOnActivate', true)) {
        return;
    }
    if (configuration.get('languageServerPath')?.trim()) {
        return;
    }
    const projectPath = resolveLanguageServerProjectPath();
    if (!projectPath) {
        return;
    }
    if (languageServerBuildPromise) {
        return languageServerBuildPromise;
    }
    languageServerBuildPromise = (async () => {
        const projectDirectory = path.dirname(projectPath);
        const args = ['build', projectPath, '/property:WarningLevel=0'];
        appendLifecycleLog(`Building language server: dotnet ${args.join(' ')}`);
        try {
            const { stdout, stderr } = await execFileText('dotnet', args, {
                cwd: projectDirectory,
                maxBuffer: 10 * 1024 * 1024
            });
            if (stdout.trim().length > 0) {
                output.appendLine(stdout);
            }
            if (stderr.trim().length > 0) {
                output.appendLine(stderr);
            }
            appendLifecycleLog('Language server build completed.');
        }
        catch (error) {
            const e = error;
            if (e.stdout)
                output.appendLine(e.stdout);
            if (e.stderr)
                output.appendLine(e.stderr);
            const message = `Failed to build Raven language server before activation. ${e.message}`;
            appendLifecycleLog(message);
            void vscode.window.showErrorMessage(message);
            throw new Error(message);
        }
    })().finally(() => {
        languageServerBuildPromise = undefined;
    });
    return languageServerBuildPromise;
}
function createLanguageClient(context) {
    let serverPath;
    try {
        serverPath = resolveServerPath(context, output);
    }
    catch (e) {
        const message = e instanceof Error ? e.message : String(e);
        output.appendLine(message);
        output.show(true);
        throw new Error(`Raven: ${message}`);
    }
    let isolatedServerPath;
    try {
        isolatedServerPath = stageServerForIsolatedLaunch(context, serverPath);
    }
    catch (e) {
        const message = e instanceof Error ? e.message : String(e);
        output.appendLine(`Failed to stage isolated language server: ${message}`);
        output.show(true);
        throw new Error(`Raven: Failed to stage isolated language server: ${message}`);
    }
    output.appendLine(`Using language server: ${serverPath}`);
    output.appendLine(`Using isolated language server: ${isolatedServerPath}`);
    const languageServerWorkingDirectory = tryFindRepositoryRoot(serverPath) ?? path.dirname(isolatedServerPath);
    output.appendLine(`Using language server working directory: ${languageServerWorkingDirectory}`);
    const runCommand = {
        command: 'dotnet',
        args: [isolatedServerPath],
        options: {
            cwd: languageServerWorkingDirectory
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
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{rvn,rav,rvnproj,csproj,fsproj}')
        },
        outputChannel: output,
        traceOutputChannel: output,
        errorHandler: {
            error(error, message, count) {
                const errorMessage = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
                const messageSummary = message ? JSON.stringify(message) : '<none>';
                appendLifecycleLog(`Language client transport error: error=${errorMessage} message=${messageSummary} count=${count ?? '<none>'}`);
                return { action: node_1.ErrorAction.Continue };
            },
            closed() {
                appendLifecycleLog('Language client transport closed. Requesting restart.');
                return { action: node_1.CloseAction.Restart };
            }
        },
        middleware: {
            async sendRequest(type, param, token, next) {
                const method = formatRequestType(type);
                const interesting = method === 'textDocument/hover' ||
                    method === 'textDocument/completion' ||
                    method === 'textDocument/inlayHint' ||
                    method === 'textDocument/semanticTokens/full' ||
                    method === 'textDocument/semanticTokens/range' ||
                    method === 'textDocument/documentSymbol' ||
                    method === 'textDocument/documentDiagnostic' ||
                    method === 'workspace/diagnostic';
                const startedAt = Date.now();
                const target = interesting ? formatRequestTarget(param) : '';
                if (interesting) {
                    appendLifecycleLog(`Request started: ${method}${target}`);
                }
                if (isSemanticTokensRequest(method) && !areSemanticTokensEnabled()) {
                    appendLifecycleLog(`Request completed: ${method}${target} in 0ms. semantic tokens disabled by raven.semanticTokens.enabled`);
                    return { data: [] };
                }
                try {
                    const result = await next(type, param, token);
                    if (interesting) {
                        appendLifecycleLog(`Request completed: ${method}${target} in ${Date.now() - startedAt}ms.${formatRequestResult(method, result)}`);
                    }
                    return result;
                }
                catch (error) {
                    const message = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
                    if (interesting) {
                        appendLifecycleLog(`Request failed: ${method}${target} after ${Date.now() - startedAt}ms: ${message}`);
                    }
                    throw error;
                }
            },
            async sendNotification(type, next, params) {
                const method = formatRequestType(type);
                const interesting = method === 'textDocument/didOpen' ||
                    method === 'textDocument/didChange' ||
                    method === 'textDocument/didSave' ||
                    method === 'textDocument/didClose';
                if (interesting) {
                    appendLifecycleLog(`Notification sent: ${method}${formatRequestTarget(params)}`);
                }
                return next(type, params);
            },
            async provideInlayHints(document, viewPort, token, next) {
                if (document.languageId === 'raven') {
                    if (!areRavenInlayHintsEnabled()) {
                        return [];
                    }
                    const showInferredTypes = areInferredTypeInlayHintsEnabled();
                    const showNames = areNameInlayHintsEnabled();
                    if (!showInferredTypes && !showNames) {
                        return [];
                    }
                    const key = document.uri.toString();
                    const requestVersion = bumpInlayHintRequestVersion(key);
                    const shouldContinue = await waitForInlayHintQuietPeriod(document, token);
                    if (!shouldContinue) {
                        throw new vscode.CancellationError();
                    }
                    if (!isCurrentInlayHintRequest(key, requestVersion)) {
                        throw new vscode.CancellationError();
                    }
                    const hints = await next(document, viewPort, token);
                    if (!isCurrentInlayHintRequest(key, requestVersion)) {
                        throw new vscode.CancellationError();
                    }
                    if (!hints) {
                        return hints;
                    }
                    if (showInferredTypes && showNames) {
                        return hints;
                    }
                    return hints.filter(hint => {
                        if (hint.kind === vscode.InlayHintKind.Type) {
                            return showInferredTypes;
                        }
                        if (hint.kind === vscode.InlayHintKind.Parameter) {
                            return showNames;
                        }
                        return true;
                    });
                }
                return next(document, viewPort, token);
            }
        }
    };
    const createdClient = new node_1.LanguageClient('ravenLanguageServer', 'Raven Language Server', serverOptions, clientOptions);
    createdClient.onDidChangeState(logStateChange);
    createdClient.setTrace(node_1.Trace.Verbose);
    appendLifecycleLog('Language client trace level set to Verbose.');
    return createdClient;
}
async function startClient(context, reason) {
    if (clientStartPromise) {
        appendLifecycleLog(`startClient(${reason}) joined existing start operation.`);
        return clientStartPromise;
    }
    clientStartPromise = (async () => {
        if (client) {
            appendLifecycleLog(`startClient(${reason}) skipped: client already active.`);
            return;
        }
        await ensureLanguageServerBuilt();
        try {
            client = createLanguageClient(context);
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            void vscode.window.showErrorMessage(message);
            throw error;
        }
        appendLifecycleLog(`Starting language client (${reason}).`);
        await client.start();
        client.onRequest(node_1.InlayHintRefreshRequest.type, async () => {
            appendLifecycleLog('Received workspace/inlayHint/refresh from language server.');
            await refreshInlayHints();
        });
    })().finally(() => {
        clientStartPromise = undefined;
    });
    return clientStartPromise;
}
async function restartClient(context, reason) {
    appendLifecycleLog(`restartClient(${reason}) requested.`);
    await stopClient(`restart:${reason}`);
    await startClient(context, `restart:${reason}`);
}
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
function resolveConfiguredSdkPath() {
    const configuration = vscode.workspace.getConfiguration('raven');
    const configuredPath = configuration.get('sdkPath')?.trim();
    if (!configuredPath) {
        return undefined;
    }
    const absolutePath = path.isAbsolute(configuredPath)
        ? configuredPath
        : path.resolve(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? extensionInstallPath, configuredPath);
    return fs.existsSync(absolutePath) ? absolutePath : undefined;
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
    const sdkPath = resolveConfiguredSdkPath();
    if (sdkPath) {
        const sdkCandidates = [
            path.join(sdkPath, 'Raven.LanguageServer.dll'),
            path.join(sdkPath, 'server', 'Raven.LanguageServer.dll'),
            path.join(sdkPath, 'net10.0', 'Raven.LanguageServer.dll'),
            path.join(sdkPath, 'net11.0', 'Raven.LanguageServer.dll')
        ];
        for (const candidate of sdkCandidates) {
            attempts.push(candidate);
            if (fs.existsSync(candidate)) {
                return candidate;
            }
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
function createStableHash(input) {
    return crypto.createHash('sha256').update(input).digest('hex').slice(0, 12);
}
function createDirectoryFingerprint(directoryPath) {
    const entries = [];
    function visit(currentDirectory) {
        for (const entry of fs.readdirSync(currentDirectory, { withFileTypes: true })) {
            const fullPath = path.join(currentDirectory, entry.name);
            const relativePath = path.relative(directoryPath, fullPath).split(path.sep).join('/');
            if (entry.isDirectory()) {
                visit(fullPath);
                continue;
            }
            if (!entry.isFile()) {
                continue;
            }
            const stat = fs.statSync(fullPath);
            entries.push(`${relativePath}|${stat.mtimeMs}|${stat.size}`);
        }
    }
    visit(directoryPath);
    entries.sort();
    return createStableHash(entries.join('\n'));
}
function stageServerForIsolatedLaunch(context, sourceServerPath) {
    const sourceDirectory = path.dirname(sourceServerPath);
    const fingerprint = createStableHash(`${sourceDirectory}|${createDirectoryFingerprint(sourceDirectory)}`);
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
function tryFindRepositoryRoot(startPath) {
    let current = fs.statSync(startPath).isDirectory()
        ? path.resolve(startPath)
        : path.dirname(path.resolve(startPath));
    while (true) {
        if (fs.existsSync(path.join(current, 'Raven.sln'))) {
            return current;
        }
        const parent = path.dirname(current);
        if (parent === current) {
            return undefined;
        }
        current = parent;
    }
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
function resolveFrontendProjectPath() {
    const searchRoots = new Set();
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
        searchRoots.add(folder.uri.fsPath);
    }
    if (extensionInstallPath.length > 0) {
        searchRoots.add(extensionInstallPath);
    }
    const compilerProjectPath = resolveCompilerProjectPath();
    if (compilerProjectPath) {
        searchRoots.add(path.dirname(compilerProjectPath));
    }
    searchRoots.add(process.cwd());
    for (const root of searchRoots) {
        for (const dir of enumerateAncestorDirectories(root)) {
            const candidate = path.join(dir, 'src', 'Raven', 'Raven.csproj');
            if (fs.existsSync(candidate)) {
                return candidate;
            }
        }
    }
    return undefined;
}
function resolveCompilerInvocation(targetFramework) {
    const sdkPath = resolveConfiguredSdkPath();
    const bundledRoots = extensionInstallPath.length > 0
        ? [
            path.join(extensionInstallPath, 'compiler'),
            path.join(extensionInstallPath, 'server')
        ]
        : [];
    const preferredTfms = targetFramework
        ? [targetFramework, 'net10.0', 'net11.0']
        : ['net10.0', 'net11.0'];
    if (sdkPath) {
        const sdkRoots = [
            sdkPath,
            path.join(sdkPath, 'compiler'),
            path.join(sdkPath, 'server')
        ];
        for (const root of sdkRoots) {
            for (const tfm of preferredTfms) {
                const tfmCandidate = path.join(root, tfm, 'rvnc.dll');
                if (fs.existsSync(tfmCandidate)) {
                    return {
                        executable: 'dotnet',
                        args: [tfmCandidate],
                        description: tfmCandidate
                    };
                }
            }
            const flatCandidate = path.join(root, 'rvnc.dll');
            if (fs.existsSync(flatCandidate)) {
                return {
                    executable: 'dotnet',
                    args: [flatCandidate],
                    description: flatCandidate
                };
            }
        }
    }
    for (const root of bundledRoots) {
        for (const tfm of preferredTfms) {
            const tfmCandidate = path.join(root, tfm, 'rvnc.dll');
            if (fs.existsSync(tfmCandidate)) {
                return {
                    executable: 'dotnet',
                    args: [tfmCandidate],
                    description: tfmCandidate
                };
            }
        }
        const flatCandidate = path.join(root, 'rvnc.dll');
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
        const candidate = path.join(compilerDirectory, 'bin', 'Debug', tfm, 'rvnc.dll');
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
function resolveFrontendInvocation(targetFramework) {
    const sdkPath = resolveConfiguredSdkPath();
    const bundledRoots = extensionInstallPath.length > 0
        ? [
            path.join(extensionInstallPath, 'tools'),
            path.join(extensionInstallPath, 'compiler'),
            path.join(extensionInstallPath, 'server')
        ]
        : [];
    const preferredTfms = targetFramework
        ? [targetFramework, 'net10.0', 'net11.0']
        : ['net10.0', 'net11.0'];
    if (sdkPath) {
        const sdkRoots = [
            sdkPath,
            path.join(sdkPath, 'tools'),
            path.join(sdkPath, 'compiler'),
            path.join(sdkPath, 'server')
        ];
        for (const root of sdkRoots) {
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
    }
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
    const frontendProjectPath = resolveFrontendProjectPath();
    if (!frontendProjectPath) {
        return undefined;
    }
    const frontendDirectory = path.dirname(frontendProjectPath);
    for (const tfm of preferredTfms) {
        const candidate = path.join(frontendDirectory, 'bin', 'Debug', tfm, 'rvn.dll');
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
function getTfmPathSegment(targetFramework, fallback) {
    const tfm = targetFramework?.trim();
    return tfm && tfm.length > 0 ? tfm : fallback;
}
function resolveOutputLayout(targetPath, configuration) {
    const effectiveTargetPath = resolveEffectiveTargetPath(targetPath);
    const targetFramework = resolveTargetFramework(effectiveTargetPath);
    const targetIsProject = isRavenProjectFile(effectiveTargetPath);
    const workspaceFolder = getContainingWorkspaceFolderPath(effectiveTargetPath);
    if (targetIsProject) {
        const projectDirectory = path.dirname(effectiveTargetPath);
        const tfmSegment = getTfmPathSegment(targetFramework, 'unknown-tfm');
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
    const tfmSegment = getTfmPathSegment(targetFramework, 'no-tfm');
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
    const layout = resolveOutputLayout(targetPath, 'Debug');
    const invocation = layout.targetIsProject
        ? resolveFrontendInvocation(layout.targetFramework)
        : resolveCompilerInvocation(layout.targetFramework);
    if (!invocation) {
        throw new Error(layout.targetIsProject
            ? 'Unable to locate a built rvn.dll. Build Raven first or set "raven.sdkPath" to an SDK directory containing rvn.dll.'
            : 'Unable to locate a built rvnc.dll. Build Raven.Compiler first or point "raven.compilerProjectPath" at a workspace containing src/Raven.Compiler/bin/Debug/<tfm>/rvnc.dll.');
    }
    if (!layout.targetIsProject) {
        if (fs.existsSync(layout.outputDirectory)) {
            fs.rmSync(layout.outputDirectory, { recursive: true, force: true });
        }
        fs.mkdirSync(layout.outputDirectory, { recursive: true });
    }
    const dotnetArgs = layout.targetIsProject
        ? [
            ...invocation.args,
            'build',
            layout.effectiveTargetPath,
            '--configuration',
            'Debug',
            ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
        ]
        : [
            ...invocation.args,
            layout.effectiveTargetPath,
            '-o',
            layout.outputDllPath,
            ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
        ];
    output.appendLine(`Compiling for debug via ${invocation.description}: ${invocation.executable} ${dotnetArgs.join(' ')}`);
    try {
        const { stdout, stderr } = await execFileText(invocation.executable, dotnetArgs, {
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
    const layout = resolveOutputLayout(targetPath, 'Debug');
    const invocation = layout.targetIsProject
        ? resolveFrontendInvocation(layout.targetFramework)
        : resolveCompilerInvocation(layout.targetFramework);
    if (!invocation) {
        throw new Error(layout.targetIsProject
            ? 'Unable to locate a built rvn.dll. Build Raven first or set "raven.sdkPath" to an SDK directory containing rvn.dll.'
            : 'Unable to locate a built rvnc.dll. Build Raven.Compiler first or point "raven.compilerProjectPath" at a workspace containing src/Raven.Compiler/bin/Debug/<tfm>/rvnc.dll.');
    }
    if (!layout.targetIsProject) {
        fs.mkdirSync(layout.outputDirectory, { recursive: true });
    }
    const dotnetArgs = layout.targetIsProject
        ? [
            ...invocation.args,
            'build',
            layout.effectiveTargetPath,
            '--configuration',
            'Debug',
            ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
        ]
        : [
            ...invocation.args,
            layout.effectiveTargetPath,
            '-o',
            layout.outputDllPath,
            ...(layout.targetFramework ? ['--framework', layout.targetFramework] : [])
        ];
    output.appendLine(`Building Raven target via ${invocation.description}: ${invocation.executable} ${dotnetArgs.join(' ')}`);
    try {
        const { stdout, stderr } = await execFileText(invocation.executable, dotnetArgs, {
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
    appendLifecycleLog(`Extension activate() called. extensionPath=${context.extensionPath}`);
    // Ensure VS Code disposes the client on shutdown.
    context.subscriptions.push({
        dispose: () => {
            appendLifecycleLog('Extension subscription dispose() called.');
            return stopClient('subscription dispose');
        }
    });
    void startClient(context, 'activate');
    context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration('raven.inlayHints.enabled') ||
            event.affectsConfiguration('raven.inlayHints.inferredTypes.enabled') ||
            event.affectsConfiguration('raven.inlayHints.names.enabled')) {
            void refreshInlayHints();
        }
    }));
    context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(event => {
        if (event.document.languageId !== 'raven') {
            return;
        }
        recentRavenDocumentChanges.set(event.document.uri.toString(), {
            version: event.document.version,
            changedAt: Date.now()
        });
        invalidateVisibleInlayHintRequests();
        if (areRavenInlayHintsEnabled() &&
            (areInferredTypeInlayHintsEnabled() || areNameInlayHintsEnabled()) &&
            event.contentChanges.length > 0) {
            scheduleInlayHintRefresh();
        }
        if (shouldTriggerImportCompletionAfterQuietPeriod(event)) {
            scheduleImportCompletionTrigger(event.document);
        }
    }));
    context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(document => {
        if (document.languageId === 'raven') {
            recentRavenDocumentChanges.delete(document.uri.toString());
            inlayHintRequestVersions.delete(document.uri.toString());
        }
    }));
    context.subscriptions.push(vscode.workspace.onDidChangeWorkspaceFolders(event => {
        const added = event.added.map(folder => folder.uri.fsPath).join(', ') || '<none>';
        const removed = event.removed.map(folder => folder.uri.fsPath).join(', ') || '<none>';
        appendLifecycleLog(`Workspace folders changed. added=${added} removed=${removed}`);
        void restartClient(context, 'workspace folders changed');
    }));
    const debugConfigurationProvider = new RavenDebugConfigurationProvider();
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('raven', debugConfigurationProvider, vscode.DebugConfigurationProviderTriggerKind.Dynamic));
    context.subscriptions.push(vscode.commands.registerCommand('raven.toggleInlayHints', async () => {
        const configuration = vscode.workspace.getConfiguration('raven');
        const current = configuration.get('inlayHints.enabled', true);
        const next = !current;
        await configuration.update('inlayHints.enabled', next, vscode.ConfigurationTarget.Global);
        await refreshInlayHints();
        void vscode.window.showInformationMessage(`Raven inlay hints ${next ? 'enabled' : 'disabled'}.`);
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.toggleInferredTypeInlayHints', async () => {
        const configuration = vscode.workspace.getConfiguration('raven');
        const current = configuration.get('inlayHints.inferredTypes.enabled', true);
        const next = !current;
        await configuration.update('inlayHints.inferredTypes.enabled', next, vscode.ConfigurationTarget.Global);
        await refreshInlayHints();
        void vscode.window.showInformationMessage(`Raven inferred type inlay hints ${next ? 'enabled' : 'disabled'}.`);
    }));
    context.subscriptions.push(vscode.commands.registerCommand('raven.toggleNameInlayHints', async () => {
        const configuration = vscode.workspace.getConfiguration('raven');
        const current = configuration.get('inlayHints.names.enabled', true);
        const next = !current;
        await configuration.update('inlayHints.names.enabled', next, vscode.ConfigurationTarget.Global);
        await refreshInlayHints();
        void vscode.window.showInformationMessage(`Raven name inlay hints ${next ? 'enabled' : 'disabled'}.`);
    }));
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
    appendLifecycleLog('Extension deactivate() called.');
    return stopClient('deactivate');
}
