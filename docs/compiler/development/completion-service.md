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

## Current coverage snapshots
- Member and namespace access is driven by semantic lookup: after a dot the provider filters accessible static, instance, and extension members, while also supporting qualified names and `self` access when available.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L526-L663】
- Import and alias directives surface namespace/type symbols from the binder and respect partial prefixes, with separate handling for alias targets and alias identifiers.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L280-L401】
- Literal completions are inferred from the target type of assignments or initializer expressions (including union and null literals), so only literals valid for the target type are offered.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L144-L201】
- The current keyword list is intentionally small (control-flow and declaration basics plus literals), meaning most reserved/contextual keywords are not suggested by completion.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L664-L676】

## Opportunities to expand syntax support
- Broaden keyword offerings to include the remaining reserved/contextual tokens defined in `Tokens.xml` (e.g., declaration keywords like `func`, `enum`, `union`, `struct`, `class`, control-flow keywords like `match`, `try`/`catch`/`finally`, `await`, `yield`, and modifiers such as `public`, `private`, `async`, `override`).【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L664-L676】【F:src/Raven.CodeAnalysis/Syntax/Tokens.xml†L4-L72】 Context-aware filtering (statement vs declaration positions) would reduce noise while making completions align with the language spec.
- Offer pattern-oriented suggestions (e.g., `when` guards, `is`/`as` operators, `default`, `typeof`) when the caret is inside match arms or relational patterns, leveraging the keyword set already tokenized by the lexer.【F:src/Raven.CodeAnalysis/Syntax/Tokens.xml†L45-L52】
- Support object initializer property suggestions, which is currently stubbed out; wiring this path would surface settable properties for the constructed type inside initializer braces.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L469-L494】
- Restore attribute type completions so users see attribute names (with the `Attribute` suffix trimmed) when typing inside attribute lists.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L496-L520】
- Provide declaration-intent completions after modifiers (e.g., after `public` suggest `class`, `func`, `struct`, `extension`) using the token set from the lexer to accelerate top-level authoring.【F:src/Raven.CodeAnalysis/Syntax/Tokens.xml†L15-L72】

## Opportunities to expand symbol support
- Within invocation argument lists, suggest named arguments derived from the target method or constructor parameters to reduce errors when calling APIs with multiple optional parameters. No such path exists today in the provider.
- When constructing objects, include accessible constructors directly (with signatures) or suggest factory patterns (static `Create` methods) to help users pick the right overload, complementing the existing `new` keyword completions that only surface instantiable types.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L403-L420】
- Enhance completions for `goto` by prioritizing labels within the current block or loop scope and differentiating between `goto` statements and switch-like constructs for better signal-to-noise.【F:src/Raven.CodeAnalysis/CompletionProvider.cs†L430-L467】
