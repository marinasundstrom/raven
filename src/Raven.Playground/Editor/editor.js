import * as monaco from "monaco-editor/editor/editor.main.js";
import { INITIAL, Registry } from "vscode-textmate";
import { loadWASM, OnigScanner, OnigString } from "vscode-oniguruma";
import onigurumaWasmUrl from "vscode-oniguruma/release/onig.wasm";
import ravenGrammar from "../../Raven.VSCode/syntaxes/raven.tmLanguage.json";

self.MonacoEnvironment = {
  getWorker: () => new Worker(new URL("./editor.worker.js", import.meta.url), { type: "module" }),
};

let grammarPromise;

async function getGrammar() {
  if (!grammarPromise) {
    grammarPromise = (async () => {
      const wasmUrl = new URL(onigurumaWasmUrl, import.meta.url);
      await loadWASM(await (await fetch(wasmUrl)).arrayBuffer());

      const registry = new Registry({
        onigLib: Promise.resolve({
          createOnigScanner: patterns => new OnigScanner(patterns),
          createOnigString: value => new OnigString(value),
        }),
        loadGrammar: scopeName =>
          scopeName === "source.raven" ? Promise.resolve(ravenGrammar) : null,
      });

      return registry.loadGrammar("source.raven");
    })();
  }

  return grammarPromise;
}

class TextMateState {
  constructor(ruleStack = INITIAL) {
    this.ruleStack = ruleStack;
  }

  clone() {
    return new TextMateState(this.ruleStack);
  }

  equals(other) {
    return other instanceof TextMateState && this.ruleStack.equals(other.ruleStack);
  }
}

function tokenType(scopes) {
  const scope = scopes.join(" ");

  if (scope.includes("invalid.")) return "invalid";
  if (scope.includes("comment")) return "comment";
  if (scope.includes("string")) return "string";
  if (scope.includes("constant.numeric")) return "number";
  if (scope.includes("constant.language")) return "constant";
  if (scope.includes("keyword") || scope.includes("storage.")) return "keyword";
  if (scope.includes("entity.name.type") || scope.includes("support.type")) return "type";
  if (scope.includes("entity.name.function") || scope.includes("support.function")) return "function";
  if (scope.includes("variable.parameter")) return "parameter";
  if (scope.includes("operator")) return "operator";
  if (scope.includes("punctuation")) return "delimiter";

  return "";
}

async function registerRavenLanguage() {
  monaco.languages.register({
    id: "raven",
    extensions: [".rav", ".rvn"],
    aliases: ["Raven", "raven"],
  });

  monaco.languages.setLanguageConfiguration("raven", {
    comments: { lineComment: "//", blockComment: ["/*", "*/"] },
    brackets: [["{", "}"], ["[", "]"], ["(", ")"]],
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: "\"", close: "\"" },
    ],
  });

  const grammar = await getGrammar();
  monaco.languages.setTokensProvider("raven", {
    getInitialState: () => new TextMateState(),
    tokenize: (line, state) => {
      const result = grammar.tokenizeLine(line, state.ruleStack);
      return {
        endState: new TextMateState(result.ruleStack),
        tokens: result.tokens.map(token => ({
          startIndex: token.startIndex,
          scopes: tokenType(token.scopes),
        })),
      };
    },
  });
}

let registrationPromise;

export async function setSharedSource(encodedSource) {
  const url = new URL(window.location.href);
  url.searchParams.set("source", encodedSource);
  url.searchParams.delete("example");
  url.searchParams.delete("snippet");
  url.searchParams.delete("run");
  window.history.replaceState(null, "", url);

  try {
    await navigator.clipboard.writeText(url.href);
    return true;
  } catch {
    return false;
  }
}

export function setSelectedExample(exampleId) {
  const url = new URL(window.location.href);
  url.searchParams.set("example", exampleId);
  url.searchParams.delete("source");
  url.searchParams.delete("snippet");
  url.searchParams.delete("run");
  window.history.replaceState(null, "", url);
}

export function getThemeMode() {
  return localStorage.getItem("raven-theme") ?? "system";
}

export function setThemeMode(mode) {
  const normalized = ["system", "light", "dark"].includes(mode) ? mode : "system";
  localStorage.setItem("raven-theme", normalized);
  document.documentElement.dataset.theme = normalized;
  window.dispatchEvent(new CustomEvent("raven-theme-change", { detail: normalized }));
}

const completionKinds = {
  class: monaco.languages.CompletionItemKind.Class,
  constructor: monaco.languages.CompletionItemKind.Constructor,
  enum: monaco.languages.CompletionItemKind.Enum,
  event: monaco.languages.CompletionItemKind.Event,
  field: monaco.languages.CompletionItemKind.Field,
  function: monaco.languages.CompletionItemKind.Function,
  interface: monaco.languages.CompletionItemKind.Interface,
  keyword: monaco.languages.CompletionItemKind.Keyword,
  method: monaco.languages.CompletionItemKind.Method,
  module: monaco.languages.CompletionItemKind.Module,
  property: monaco.languages.CompletionItemKind.Property,
  struct: monaco.languages.CompletionItemKind.Struct,
  text: monaco.languages.CompletionItemKind.Text,
  typeParameter: monaco.languages.CompletionItemKind.TypeParameter,
  variable: monaco.languages.CompletionItemKind.Variable,
};

function toSnippetText(insertText, cursorOffset) {
  if (cursorOffset == null || cursorOffset >= insertText.length) return insertText;

  const escapeSnippet = text =>
    text.replaceAll("\\", "\\\\").replaceAll("$", "\\$").replaceAll("}", "\\}");
  return `${escapeSnippet(insertText.slice(0, cursorOffset))}$0${escapeSnippet(insertText.slice(cursorOffset))}`;
}

function shouldTriggerAutomaticCompletion(model, position) {
  const linePrefix = model.getLineContent(position.lineNumber).slice(0, position.column - 1);
  const identifier = /[A-Za-z_][A-Za-z0-9_]*$/.exec(linePrefix)?.[0];
  if (!identifier || identifier.length < 3) return false;

  const tokens = monaco.editor.tokenize(linePrefix, "raven")[0] ?? [];
  const offset = Math.max(0, linePrefix.length - 1);
  let token;
  for (const candidate of tokens) {
    if (candidate.offset > offset) break;
    token = candidate;
  }
  return !token?.type.includes("comment") && !token?.type.includes("string");
}

function registerRavenThemes() {
  const tokenRules = isDark => [
    { token: "comment", foreground: isDark ? "6A9955" : "008000", fontStyle: "italic" },
    { token: "string", foreground: isDark ? "CE9178" : "A31515" },
    { token: "number", foreground: isDark ? "B5CEA8" : "098658" },
    { token: "constant", foreground: isDark ? "569CD6" : "0000FF" },
    { token: "keyword", foreground: isDark ? "569CD6" : "0000FF" },
    { token: "type", foreground: isDark ? "4EC9B0" : "267F99" },
    { token: "function", foreground: isDark ? "DCDCAA" : "795E26" },
    { token: "parameter", foreground: isDark ? "9CDCFE" : "001080" },
    { token: "operator", foreground: isDark ? "D4D4D4" : "000000" },
    { token: "invalid", foreground: isDark ? "F44747" : "CD3131" },
  ];

  monaco.editor.defineTheme("raven-light", {
    base: "vs",
    inherit: true,
    rules: tokenRules(false),
    colors: {
      "editor.background": "#FFFFFF",
      "editor.foreground": "#000000",
      "editor.lineHighlightBackground": "#F7F7F7",
      "editorLineNumber.foreground": "#858585",
      "editorLineNumber.activeForeground": "#0B216F",
      "editor.selectionBackground": "#ADD6FF",
      "editorCursor.foreground": "#000000",
      "editorSuggestWidget.background": "#F3F3F3",
      "editorSuggestWidget.border": "#C8C8C8",
      "editorSuggestWidget.selectedBackground": "#D6EBFF",
    },
  });
  monaco.editor.defineTheme("raven-dark", {
    base: "vs-dark",
    inherit: true,
    rules: tokenRules(true),
    colors: {
      "editor.background": "#1E1E1E",
      "editor.foreground": "#D4D4D4",
      "editor.lineHighlightBackground": "#2A2D2E",
      "editorLineNumber.foreground": "#858585",
      "editorLineNumber.activeForeground": "#C6C6C6",
      "editor.selectionBackground": "#264F78",
      "editorCursor.foreground": "#AEAFAD",
      "editorSuggestWidget.background": "#252526",
      "editorSuggestWidget.border": "#454545",
      "editorSuggestWidget.selectedBackground": "#04395E",
    },
  });
}

export async function createEditor(element, value, commandTarget) {
  registrationPromise ??= registerRavenLanguage();
  await registrationPromise;
  registerRavenThemes();
  const colorScheme = window.matchMedia("(prefers-color-scheme: dark)");
  const isDarkTheme = () => {
    const mode = document.documentElement.dataset.theme ?? "system";
    return mode === "dark" || (mode === "system" && colorScheme.matches);
  };
  const applyTheme = () =>
    monaco.editor.setTheme(isDarkTheme() ? "raven-dark" : "raven-light");
  applyTheme();
  colorScheme.addEventListener("change", applyTheme);
  window.addEventListener("raven-theme-change", applyTheme);

  const model = monaco.editor.createModel(
    value,
    "raven",
    monaco.Uri.parse("inmemory://raven/main.rav"),
  );
  const editor = monaco.editor.create(element, {
    model,
    automaticLayout: true,
    fontFamily: "'Berkeley Mono', 'SFMono-Regular', Consolas, monospace",
    fontSize: 14,
    lineHeight: 22,
    minimap: { enabled: false },
    padding: { top: 16, bottom: 16 },
    quickSuggestions: false,
    renderLineHighlight: "line",
    scrollBeyondLastLine: false,
    suggestOnTriggerCharacters: false,
    tabSize: 4,
    theme: isDarkTheme() ? "raven-dark" : "raven-light",
  });
  const completionProvider = monaco.languages.registerCompletionItemProvider("raven", {
    provideCompletionItems: async (completionModel, position, _context, cancellationToken) => {
      const source = completionModel.getValue();
      const offset = completionModel.getOffsetAt(position);
      const items = await commandTarget.invokeMethodAsync("GetCompletions", source, offset);

      if (cancellationToken.isCancellationRequested) return { suggestions: [] };

      return {
        suggestions: items.map(item => {
          const start = completionModel.getPositionAt(item.start);
          const end = completionModel.getPositionAt(item.start + item.length);
          const usesSnippet = item.cursorOffset != null && item.cursorOffset < item.insertText.length;

          return {
            label: item.label,
            insertText: usesSnippet
              ? toSnippetText(item.insertText, item.cursorOffset)
              : item.insertText,
            insertTextRules: usesSnippet
              ? monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
              : undefined,
            detail: item.detail,
            kind: completionKinds[item.kind] ?? monaco.languages.CompletionItemKind.Text,
            range: new monaco.Range(
              start.lineNumber,
              start.column,
              end.lineNumber,
              end.column,
            ),
          };
        }),
      };
    },
  });
  let completionTimer;
  const completionTrigger = editor.onDidType(text => {
    clearTimeout(completionTimer);
    if (!/[A-Za-z0-9_]/.test(text)) return;

    const position = editor.getPosition();
    if (!shouldTriggerAutomaticCompletion(model, position)) return;

    completionTimer = setTimeout(() => {
      editor.trigger("raven.completion", "editor.action.triggerSuggest", {});
    }, 350);
  });

  editor.addAction({
    id: "raven.compile",
    label: "Compile Raven program",
    keybindings: [monaco.KeyCode.F6],
    run: () => commandTarget.invokeMethodAsync("InvokeEditorCommand", "compile"),
  });
  editor.addAction({
    id: "raven.run",
    label: "Run Raven program",
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
    run: () => commandTarget.invokeMethodAsync("InvokeEditorCommand", "run"),
  });

  return {
    getValue: () => editor.getValue(),
    setValue: source => {
      editor.setValue(source);
      editor.focus();
    },
    focus: () => editor.focus(),
    dispose: () => {
      clearTimeout(completionTimer);
      completionTrigger.dispose();
      completionProvider.dispose();
      colorScheme.removeEventListener("change", applyTheme);
      window.removeEventListener("raven-theme-change", applyTheme);
      editor.dispose();
      model.dispose();
    },
  };
}
