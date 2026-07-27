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
exports.ExpandedSyntaxContentProvider = exports.SyntaxTreeDataProvider = exports.SyntaxTreeItem = void 0;
exports.revealSyntaxTreeItem = revealSyntaxTreeItem;
const vscode = __importStar(require("vscode"));
class SyntaxTreeItem extends vscode.TreeItem {
    constructor(element, documentUri, documentVersion) {
        super(createLabel(element), element.children.length > 0
            ? (element.propertyName
                ? vscode.TreeItemCollapsibleState.Collapsed
                : vscode.TreeItemCollapsibleState.Expanded)
            : vscode.TreeItemCollapsibleState.None);
        this.element = element;
        this.documentUri = documentUri;
        this.documentVersion = documentVersion;
        this.description = createDescription(element);
        this.iconPath = createIcon(element);
        this.tooltip = createTooltip(element);
        this.contextValue = `ravenSyntaxTree.${element.category}`;
        this.command = {
            command: 'raven.syntaxTree.reveal',
            title: 'Reveal Syntax in Editor',
            arguments: [this]
        };
    }
}
exports.SyntaxTreeItem = SyntaxTreeItem;
class SyntaxTreeDataProvider {
    constructor(loader, log) {
        this.loader = loader;
        this.log = log;
        this.changeEmitter = new vscode.EventEmitter();
        this.visible = false;
        this.loadGeneration = 0;
        this.view = 'authored';
        this.onDidChangeTreeData = this.changeEmitter.event;
    }
    setVisible(visible) {
        this.visible = visible;
        if (visible) {
            this.refresh();
        }
    }
    setView(view) {
        if (this.view === view) {
            return;
        }
        this.view = view;
        this.refresh();
    }
    setActiveDocument(document) {
        if (!document ||
            document.languageId !== 'raven' ||
            document.uri.scheme === 'raven-expanded') {
            return;
        }
        if (this.activeDocument?.uri.toString() === document.uri.toString() &&
            this.activeDocument.version === document.version) {
            return;
        }
        this.activeDocument = document;
        this.scheduleRefresh(0);
    }
    getView() {
        return this.view;
    }
    getActiveDocument() {
        return this.activeDocument;
    }
    scheduleRefresh(delayMilliseconds = 250) {
        if (!this.visible) {
            return;
        }
        if (this.refreshTimer) {
            clearTimeout(this.refreshTimer);
        }
        this.refreshTimer = setTimeout(() => {
            this.refreshTimer = undefined;
            this.refresh();
        }, delayMilliseconds);
    }
    refresh() {
        this.loadGeneration++;
        this.changeEmitter.fire(undefined);
    }
    getTreeItem(element) {
        return element;
    }
    async getChildren(element) {
        if (element) {
            return element.element.children.map(child => new SyntaxTreeItem(child, element.documentUri, element.documentVersion));
        }
        try {
            if (!this.rootLoadPromise) {
                this.rootLoadPromise = this.loadLatestTree()
                    .finally(() => {
                    this.rootLoadPromise = undefined;
                });
            }
            const loadedTree = await this.rootLoadPromise;
            if (!loadedTree) {
                return [];
            }
            return [new SyntaxTreeItem(loadedTree.root, loadedTree.navigationUri, loadedTree.documentVersion)];
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            this.log(`Syntax tree visualizer failed: ${message}`);
            throw new Error(`Unable to load Raven syntax tree: ${message}`);
        }
    }
    async loadLatestTree() {
        while (true) {
            const document = this.activeDocument;
            if (!document) {
                return undefined;
            }
            const generation = this.loadGeneration;
            const documentVersion = document.version;
            const view = this.view;
            const loadedTree = await this.loader(document, view);
            if (generation === this.loadGeneration &&
                documentVersion === document.version &&
                view === this.view &&
                this.activeDocument?.uri.toString() === document.uri.toString()) {
                return {
                    ...loadedTree,
                    documentVersion
                };
            }
        }
    }
    dispose() {
        if (this.refreshTimer) {
            clearTimeout(this.refreshTimer);
            this.refreshTimer = undefined;
        }
        this.changeEmitter.dispose();
    }
}
exports.SyntaxTreeDataProvider = SyntaxTreeDataProvider;
class ExpandedSyntaxContentProvider {
    constructor() {
        this.changeEmitter = new vscode.EventEmitter();
        this.contents = new Map();
        this.onDidChange = this.changeEmitter.event;
    }
    update(sourceUri, sourceText) {
        const uri = vscode.Uri.from({
            scheme: 'raven-expanded',
            path: `${sourceUri.path}.expanded.rvn`,
            query: `source=${encodeURIComponent(sourceUri.toString())}`
        });
        this.contents.set(uri.toString(), sourceText);
        this.changeEmitter.fire(uri);
        return uri;
    }
    provideTextDocumentContent(uri) {
        return this.contents.get(uri.toString()) ?? '';
    }
    dispose() {
        this.contents.clear();
        this.changeEmitter.dispose();
    }
}
exports.ExpandedSyntaxContentProvider = ExpandedSyntaxContentProvider;
async function revealSyntaxTreeItem(item) {
    const document = await vscode.workspace.openTextDocument(item.documentUri);
    const start = document.positionAt(item.element.span.start);
    const end = document.positionAt(item.element.span.start + item.element.span.length);
    const range = new vscode.Range(start, end);
    const editor = await vscode.window.showTextDocument(document, {
        preview: false,
        preserveFocus: false
    });
    editor.selection = new vscode.Selection(start, end);
    editor.revealRange(range, item.element.span.length === 0
        ? vscode.TextEditorRevealType.InCenterIfOutsideViewport
        : vscode.TextEditorRevealType.Default);
}
function createLabel(element) {
    const propertyPrefix = element.propertyName ? `${element.propertyName}: ` : '';
    const textSuffix = element.text === undefined || element.text.length === 0
        ? ''
        : ` ${formatText(element.text)}`;
    const missingSuffix = element.isMissing ? ' (missing)' : '';
    return `${propertyPrefix}${element.kind}${textSuffix}${missingSuffix}`;
}
function createDescription(element) {
    const end = element.span.start + element.span.length;
    return `[${element.span.start}..${end})`;
}
function createIcon(element) {
    switch (element.category) {
        case 'token':
            return new vscode.ThemeIcon('symbol-key');
        case 'trivia':
            return new vscode.ThemeIcon('whitespace');
        default:
            return new vscode.ThemeIcon('symbol-structure');
    }
}
function createTooltip(element) {
    const tooltip = new vscode.MarkdownString();
    tooltip.appendMarkdown(`**${capitalize(element.category)}** \`${element.kind}\`\n\n`);
    tooltip.appendMarkdown(`Raw kind: \`${element.rawKind}\`  \n`);
    tooltip.appendMarkdown(`Span: \`${formatSpan(element.span)}\`  \n`);
    tooltip.appendMarkdown(`Full span: \`${formatSpan(element.fullSpan)}\``);
    if (element.propertyName) {
        tooltip.appendMarkdown(`  \nProperty: \`${element.propertyName}\``);
    }
    if (element.text !== undefined) {
        tooltip.appendMarkdown('\n\nText:\n');
        tooltip.appendCodeblock(element.text);
    }
    if (element.diagnostics.length > 0) {
        tooltip.appendMarkdown('\n\nDiagnostics:\n');
        for (const diagnostic of element.diagnostics) {
            tooltip.appendMarkdown(`\n- **${diagnostic.id}** (${diagnostic.severity}): ${escapeMarkdown(diagnostic.message)}`);
        }
    }
    return tooltip;
}
function formatText(text) {
    const escaped = text
        .replace(/\\/g, '\\\\')
        .replace(/\r/g, '\\r')
        .replace(/\n/g, '\\n')
        .replace(/\t/g, '\\t');
    const shortened = escaped.length > 40 ? `${escaped.slice(0, 37)}...` : escaped;
    return JSON.stringify(shortened);
}
function formatSpan(span) {
    return `[${span.start}..${span.start + span.length})`;
}
function capitalize(value) {
    return value.charAt(0).toUpperCase() + value.slice(1);
}
function escapeMarkdown(value) {
    return value.replace(/[\\`*_{}[\]()#+\-.!]/g, '\\$&');
}
