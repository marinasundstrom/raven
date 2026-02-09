# Implementing label and goto statements

This note records the gaps that currently block label and `goto` statements and lays out a sequence of concrete implementation tasks. Each section references the existing locations in the compiler that will need to change so work can be broken down into incremental pull requests.

## Current gaps

* The syntax model has no representation for labels or goto statements, so `LanguageParser` cannot recognize constructs such as `start:` or `goto start`. Attempting to bind such syntax today would fall into the default case of `BlockBinder.BindStatement`, which throws for unsupported statement kinds.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L385-L408】
* Control-flow analysis explicitly stubs out label resolution and goto tracking. `ControlFlowWalker.VisitGotoStatement` and `VisitLabeledStatement` are commented out, and `SemanticModel` only exposes placeholders for resolving labels and checking external gotos.【F:src/Raven.CodeAnalysis/SemanticModel.ControlFlowAnalysis.cs†L83-L141】【F:src/Raven.CodeAnalysis/SemanticModel.ControlFlowAnalysis.cs†L151-L169】
* No bound nodes, symbols, or code-generation paths exist for labels. The symbol layer currently lacks a `Label` kind, and the statement emitter only understands structured control flow.【F:src/Raven.CodeAnalysis/Symbols/ISymbol.cs†L10-L56】【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L18-L216】
* The language specification and diagnostics catalogue do not mention labels or goto, so user-facing behavior is undefined.【F:docs/lang/spec/language-specification.md†L130-L209】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L1-L1】

## Step-by-step implementation plan

### 1. Extend tokens, syntax kinds, and the parser

1. Introduce a `GotoKeyword` (and any contextual keywords needed) in `Tokens.xml`, then regenerate syntax facts so the lexer classifies `goto` as a keyword.【F:src/Raven.CodeAnalysis/Syntax/Tokens.xml†L1-L104】
2. Augment `Model.xml` with `LabeledStatementSyntax` and `GotoStatementSyntax` nodes. Each should expose the tokens (`Identifier`, `ColonToken`, `GotoKeyword`, `Target`, terminators) necessary for syntax tree construction.
3. Regenerate syntax node types with `tools/NodeGenerator` so strongly-typed syntax nodes become available.
4. Update `LanguageParser` to detect `identifier:` patterns when parsing statements (without stealing labels from expressions) and to parse `goto` statements, respecting Raven's newline/semicolon termination rules described in the spec.【F:docs/lang/spec/language-specification.md†L130-L209】
5. Expand `SyntaxFactory` helpers and syntax tests to account for the new statement forms.

### 2. Introduce label symbols and bound nodes

1. Add `Label` to `SymbolKind` and create a `LabelSymbol` (likely analogous to Roslyn's `LabelSymbol`) that records the label name and declaring syntax.【F:src/Raven.CodeAnalysis/Symbols/ISymbol.cs†L10-L56】
2. Ensure symbol visitors and display helpers recognize the new kind so diagnostics and IDE features can surface label names.
3. Define `BoundLabeledStatement` and `BoundGotoStatement` nodes in the bound tree. The labeled node should wrap the nested statement so control-flow rewrites can walk through labels, while the goto node should carry the `LabelSymbol` target and a flag for being forward/backward if needed.
4. Update `BoundTreeVisitor`, `BoundTreeRewriter`, and related helpers to handle the new node types.

### 3. Bind labels and goto statements

1. Extend `BlockBinder.BindStatement` to dispatch to `BindLabeledStatement`/`BindGotoStatement`. The labeled binder must declare a `LabelSymbol`, ensure uniqueness within its scope, and register it so later gotos can resolve it.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L385-L408】
2. Track label scopes so jumps cannot enter a region that skips variable initializers (`let`/`use` declarations, pattern variables, etc.). `LocalScopeBinder` and related infrastructure may need hooks to expose scope boundaries.
3. Bind goto statements by looking up the label symbol, producing diagnostics for undefined labels, collisions, or jumps across invalid boundaries (e.g., into a `finally`). Add appropriate entries to `DiagnosticDescriptors.xml` and regenerate descriptors.
4. Record goto-to-label relationships on the `SemanticModel` so later control-flow and code-generation phases can query them. This likely requires storing a per-body table mapping labels to bound nodes and referencing gotos.

### 4. Restore control-flow analysis support

1. Implement `SemanticModel.GetLabelTarget` and `HasExternalGotoToLabel` using the binder's label table so control-flow regions can reason about branch entry/exit points.【F:src/Raven.CodeAnalysis/SemanticModel.ControlFlowAnalysis.cs†L110-L169】
2. Re-enable and finalize the commented `VisitGotoStatement`/`VisitLabeledStatement` overrides in `ControlFlowWalker` to populate `EntryPoints` and `ExitPoints` for gotos that cross region boundaries.【F:src/Raven.CodeAnalysis/SemanticModel.ControlFlowAnalysis.cs†L83-L141】
3. Ensure data-flow analysis and reachability tracking mark statements after unconditional gotos as unreachable where appropriate, updating diagnostics if the compiler reports unreachable code today.

### 5. Lowering and code generation

1. Update the lowering pipeline (if any additional passes exist) to preserve labeled blocks and gotos through any transformations that currently assume structured control flow (`BoundTreeRewriter`, expression-to-statement conversions, etc.).
2. Teach `StatementGenerator` (and any expression emitters used for statement bodies) to reserve IL labels per `LabelSymbol`, mark them at the correct emission point, and emit `br` instructions for gotos. Ensure the generator consults scope-disposal metadata so jumping out of scopes still disposes pending `use` locals before branching.【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L18-L216】
3. Verify `Scope` and disposal logic continue to run when a goto leaves a scope. You may need to synthesize finally-like blocks or insert explicit dispose sequences before each branch target.

### 6. SemanticModel, symbol info, and tooling surface

1. Expose label symbols via `SemanticModel.GetDeclaredSymbol` for `LabeledStatementSyntax` and hook `GetSymbolInfo`/`LookupSymbols` so tooling can navigate to labels.
2. Update completion, formatting, and any IDE-oriented helpers that need to recognize label statements (e.g., avoid treating `identifier:` as an expression followed by a colon).

### 7. Documentation and diagnostics

1. Extend the language specification and grammar to describe label declarations and goto statements, including their allowed locations and restrictions.【F:docs/lang/spec/language-specification.md†L130-L209】
2. Document the new diagnostics (undefined label, duplicate label, invalid goto) in `docs/compiler/diagnostics.md` once descriptor IDs are reserved.
3. Add release notes or design write-ups if the feature needs broader visibility.

### 8. Testing strategy

1. Parser tests: add round-trip tests for labeled and goto statements (green and red trees) in `test/Raven.CodeAnalysis.Tests/Syntax`.
2. Binder/semantic tests: cover duplicate labels, unknown labels, illegal jumps, and symbol exposure in `Semantics` and `Diagnostics` suites.
3. Control-flow tests: extend existing control-flow analyses (once fleshed out) to ensure entry/exit points reflect gotos, mirroring Roslyn's coverage.
4. Code-generation tests: compile snippets containing forward/backward gotos, loops simulated with labels, and gotos across `use`/`try` scopes to confirm the emitted IL matches expectations.
5. End-to-end samples: add a sample to `docs` or `test/Raven.CodeAnalysis.Samples` illustrating when goto might be useful, ensuring documentation and tooling stay aligned.

Following these steps in order will gradually unlock full support for label and goto statements while keeping each change focused and testable.
