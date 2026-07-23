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

export async function createEditor(element, value, commandTarget) {
  registrationPromise ??= registerRavenLanguage();
  await registrationPromise;

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
    renderLineHighlight: "line",
    scrollBeyondLastLine: false,
    tabSize: 4,
    theme: "vs-dark",
  });
  const completionProvider = monaco.languages.registerCompletionItemProvider("raven", {
    triggerCharacters: [".", ":", "#", "[", ">"],
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
    focus: () => editor.focus(),
    dispose: () => {
      completionProvider.dispose();
      editor.dispose();
      model.dispose();
    },
  };
}
