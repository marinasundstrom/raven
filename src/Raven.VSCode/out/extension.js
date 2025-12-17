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
function resolveServerPath(context) {
    const configuration = vscode.workspace.getConfiguration('raven');
    const configuredPath = configuration.get('languageServerPath')?.trim();
    if (configuredPath && fs.existsSync(configuredPath)) {
        return configuredPath;
    }
    const packagedPath = context.asAbsolutePath(path.join('server', 'Raven.LanguageServer.dll'));
    if (fs.existsSync(packagedPath)) {
        return packagedPath;
    }
    const workspacePath = path.join(context.extensionPath, '..', 'Raven.LanguageServer', 'bin', 'Debug', 'net9.0', 'Raven.LanguageServer.dll');
    if (fs.existsSync(workspacePath)) {
        return workspacePath;
    }
    throw new Error('Unable to locate Raven.LanguageServer.dll. Build the language server or set "raven.languageServerPath" to the compiled DLL.');
}
function activate(context) {
    const serverPath = resolveServerPath(context);
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
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.rav')
        },
        outputChannel: vscode.window.createOutputChannel('Raven Language Server')
    };
    client = new node_1.LanguageClient('ravenLanguageServer', 'Raven Language Server', serverOptions, clientOptions);
    context.subscriptions.push(client);
    client.start();
}
function deactivate() {
    return client?.stop();
}
