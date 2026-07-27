import * as vscode from 'vscode';

export interface SyntaxTreeSpan {
  start: number;
  length: number;
}

export interface SyntaxTreeDiagnostic {
  id: string;
  severity: string;
  message: string;
}

export interface SyntaxTreeElement {
  category: 'node' | 'token' | 'trivia';
  kind: string;
  rawKind: number;
  propertyName: string | null;
  text: string | null;
  span: SyntaxTreeSpan;
  fullSpan: SyntaxTreeSpan;
  isMissing: boolean;
  diagnostics: SyntaxTreeDiagnostic[];
  children: SyntaxTreeElement[];
}

export type SyntaxTreeViewMode = 'authored' | 'expanded';

export interface SyntaxTreeToolDocument {
  view: SyntaxTreeViewMode;
  sourceText: string;
  root: SyntaxTreeElement;
}

export interface LoadedSyntaxTree {
  root: SyntaxTreeElement;
  navigationUri: vscode.Uri;
}

interface CurrentSyntaxTree extends LoadedSyntaxTree {
  documentVersion: number;
}

export type SyntaxTreeLoader = (
  document: vscode.TextDocument,
  view: SyntaxTreeViewMode
) => Promise<LoadedSyntaxTree>;

export class SyntaxTreeItem extends vscode.TreeItem {
  public constructor(
    public readonly element: SyntaxTreeElement,
    public readonly documentUri: vscode.Uri,
    public readonly documentVersion: number
  ) {
    super(
      createLabel(element),
      element.children.length > 0
        ? (element.propertyName
          ? vscode.TreeItemCollapsibleState.Collapsed
          : vscode.TreeItemCollapsibleState.Expanded)
        : vscode.TreeItemCollapsibleState.None
    );

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

export class SyntaxTreeDataProvider implements vscode.TreeDataProvider<SyntaxTreeItem>, vscode.Disposable {
  private readonly changeEmitter = new vscode.EventEmitter<SyntaxTreeItem | undefined>();
  private refreshTimer: NodeJS.Timeout | undefined;
  private visible = false;
  private loadGeneration = 0;
  private view: SyntaxTreeViewMode = 'authored';
  private activeDocument: vscode.TextDocument | undefined;
  private rootLoadPromise: Promise<CurrentSyntaxTree | undefined> | undefined;

  public readonly onDidChangeTreeData = this.changeEmitter.event;

  public constructor(
    private readonly loader: SyntaxTreeLoader,
    private readonly log: (message: string) => void
  ) {
  }

  public setVisible(visible: boolean): void {
    this.visible = visible;
    if (visible) {
      this.refresh();
    }
  }

  public setView(view: SyntaxTreeViewMode): void {
    if (this.view === view) {
      return;
    }

    this.view = view;
    this.refresh();
  }

  public setActiveDocument(document: vscode.TextDocument | undefined): void {
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

  public getView(): SyntaxTreeViewMode {
    return this.view;
  }

  public getActiveDocument(): vscode.TextDocument | undefined {
    return this.activeDocument;
  }

  public scheduleRefresh(delayMilliseconds = 250): void {
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

  public refresh(): void {
    this.loadGeneration++;
    this.changeEmitter.fire(undefined);
  }

  public getTreeItem(element: SyntaxTreeItem): vscode.TreeItem {
    return element;
  }

  public async getChildren(element?: SyntaxTreeItem): Promise<SyntaxTreeItem[]> {
    if (element) {
      return element.element.children.map(child =>
        new SyntaxTreeItem(child, element.documentUri, element.documentVersion));
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

      return [new SyntaxTreeItem(
        loadedTree.root,
        loadedTree.navigationUri,
        loadedTree.documentVersion
      )];
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      this.log(`Syntax tree visualizer failed: ${message}`);
      throw new Error(`Unable to load Raven syntax tree: ${message}`);
    }
  }

  private async loadLatestTree(): Promise<CurrentSyntaxTree | undefined> {
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

  public dispose(): void {
    if (this.refreshTimer) {
      clearTimeout(this.refreshTimer);
      this.refreshTimer = undefined;
    }

    this.changeEmitter.dispose();
  }
}

export class ExpandedSyntaxContentProvider implements vscode.TextDocumentContentProvider, vscode.Disposable {
  private readonly changeEmitter = new vscode.EventEmitter<vscode.Uri>();
  private readonly contents = new Map<string, string>();

  public readonly onDidChange = this.changeEmitter.event;

  public update(sourceUri: vscode.Uri, sourceText: string): vscode.Uri {
    const uri = vscode.Uri.from({
      scheme: 'raven-expanded',
      path: `${sourceUri.path}.expanded.rvn`,
      query: `source=${encodeURIComponent(sourceUri.toString())}`
    });
    this.contents.set(uri.toString(), sourceText);
    this.changeEmitter.fire(uri);
    return uri;
  }

  public provideTextDocumentContent(uri: vscode.Uri): string {
    return this.contents.get(uri.toString()) ?? '';
  }

  public dispose(): void {
    this.contents.clear();
    this.changeEmitter.dispose();
  }
}

export async function revealSyntaxTreeItem(item: SyntaxTreeItem): Promise<void> {
  const document = await vscode.workspace.openTextDocument(item.documentUri);
  const start = document.positionAt(item.element.span.start);
  const end = document.positionAt(item.element.span.start + item.element.span.length);
  const range = new vscode.Range(start, end);
  const editor = await vscode.window.showTextDocument(document, {
    preview: false,
    preserveFocus: false
  });

  editor.selection = new vscode.Selection(start, end);
  editor.revealRange(
    range,
    item.element.span.length === 0
      ? vscode.TextEditorRevealType.InCenterIfOutsideViewport
      : vscode.TextEditorRevealType.Default
  );
}

function createLabel(element: SyntaxTreeElement): string {
  const propertyPrefix = element.propertyName ? `${element.propertyName}: ` : '';
  const textSuffix = element.text === null || element.text.length === 0
    ? ''
    : ` ${formatText(element.text)}`;
  const missingSuffix = element.isMissing ? ' (missing)' : '';
  return `${propertyPrefix}${element.kind}${textSuffix}${missingSuffix}`;
}

function createDescription(element: SyntaxTreeElement): string {
  const end = element.span.start + element.span.length;
  return `[${element.span.start}..${end})`;
}

function createIcon(element: SyntaxTreeElement): vscode.ThemeIcon {
  switch (element.category) {
    case 'token':
      return new vscode.ThemeIcon('symbol-key');
    case 'trivia':
      return new vscode.ThemeIcon('whitespace');
    default:
      return new vscode.ThemeIcon('symbol-structure');
  }
}

function createTooltip(element: SyntaxTreeElement): vscode.MarkdownString {
  const tooltip = new vscode.MarkdownString();
  tooltip.appendMarkdown(`**${capitalize(element.category)}** \`${element.kind}\`\n\n`);
  tooltip.appendMarkdown(`Raw kind: \`${element.rawKind}\`  \n`);
  tooltip.appendMarkdown(`Span: \`${formatSpan(element.span)}\`  \n`);
  tooltip.appendMarkdown(`Full span: \`${formatSpan(element.fullSpan)}\``);

  if (element.propertyName) {
    tooltip.appendMarkdown(`  \nProperty: \`${element.propertyName}\``);
  }

  if (element.text !== null) {
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

function formatText(text: string): string {
  const escaped = text
    .replace(/\\/g, '\\\\')
    .replace(/\r/g, '\\r')
    .replace(/\n/g, '\\n')
    .replace(/\t/g, '\\t');
  const shortened = escaped.length > 40 ? `${escaped.slice(0, 37)}...` : escaped;
  return JSON.stringify(shortened);
}

function formatSpan(span: SyntaxTreeSpan): string {
  return `[${span.start}..${span.start + span.length})`;
}

function capitalize(value: string): string {
  return value.charAt(0).toUpperCase() + value.slice(1);
}

function escapeMarkdown(value: string): string {
  return value.replace(/[\\`*_{}[\]()#+\-.!]/g, '\\$&');
}
