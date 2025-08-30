# Completion Service

The `CompletionService` provides code completion items for the editor.
It queries the syntax tree and semantic model to suggest tokens and symbols
that are valid at a given position.

## Usage

```csharp
var service = new CompletionService();
var items = service.GetCompletions(compilation, syntaxTree, position);
```

Each returned `CompletionItem` describes a text snippet that can be inserted
at the requested position.
