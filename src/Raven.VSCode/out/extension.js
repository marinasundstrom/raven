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
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
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
function activate(context) {
    const output = vscode.window.createOutputChannel('Raven Language Server');
    output.appendLine('Activating Raven VS Code extension…');
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
}
function deactivate() {
    return client?.stop();
}
