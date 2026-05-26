---
name: raven-feature-workflow
description: End-to-end workflow for Raven language and compiler feature work. Use when adding or modifying syntax, parsing, binding, lowering, code generation, operations, language service support, or feature documentation. Covers lazy binder-owned semantic state, Roslyn-like compiler APIs, incremental compilation, language-service performance, generator rebuild decisions, docs/spec sync, changelog updates, and focused test coverage.
---

# Raven Feature Workflow

Use this skill when the task changes Raven language behavior or compiler support for a construct.

## Start

1. Inspect `docs/` to confirm intended syntax and semantics before changing behavior.
2. Identify which compiler layers are affected.
3. Decide whether `scripts/codex-build.sh` is required:
   - required after changes to syntax or bound model inputs, generator definitions, or generator config
   - otherwise prefer targeted project builds

## Feature Checklist

Walk the feature through every affected layer:

- Syntax model: update syntax definitions or models and regenerate nodes or factories if needed.
- Tokens and keywords: update token kinds, lexer handling, and keyword classification.
- Parser: parse the construct, including precedence, associativity, and recovery.
- Bound model: add or update bound nodes and generated visitor or rewriter artifacts if applicable.
- Binding and semantics: bind the construct, enforce rules, and report diagnostics instead of throwing.
- Lowering: implement behavior in lowering where appropriate; prefer lowering for new features when it keeps semantics cleaner.
- Code generation: ensure emit and runtime paths handle the feature or intentionally reject it with diagnostics.
- Operations API: update operation kinds, interfaces or nodes, factory logic, and tests or docs.
- Language service and editor: evaluate symbol lookup, hover, completion, diagnostics, and TextMate grammar coverage when relevant.
- Grammar, spec, and docs: keep `docs/` aligned with the final supported behavior; if the behavior should be documented but lacks coverage in `docs/`, add it.
- Changelog: update `CHANGELOG.md` for user-visible behavior changes.

## Compiler Architecture Direction

- Keep public semantic APIs Roslyn-like unless Raven intentionally diverges: callers should ask `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, diagnostics, operations, etc.
- Treat binders as execution units. A binder owns the derived semantic state for the syntax/scope it binds, such as method parameters, local declarations, labels, pattern variables, and binder-produced diagnostics.
- Treat `Compilation`/`SemanticModel` as the semantic service layer. Each edit should produce a stable solution/project/document snapshot, and each semantic request should run against one snapshot instead of mutable shared state.
- Treat lazy binding as the normal model. Any semantic query path may trigger a bind when the answer is not already available; after that, the resolved information should be cached in compiler-owned state and reused by later queries.
- Treat available-state APIs as opportunistic, not authoritative. They may answer from already-known compiler state, but must return no answer rather than a partial or guessed one when context is missing. Authoritative semantic APIs may bind to produce the correct answer.
- Keep semantic caching and incremental reuse inside `Raven.CodeAnalysis`. The language server, analyzers, completion, and refactorings should not depend on cache-specific helper APIs or choose invalidation policy.
- Keep analyzers and code fixes out of broad diagnostic production unless that is their explicit job. Analyzers may use `SyntaxTree.GetDiagnostics()` as a syntax-error guard, but should avoid `Compilation.GetDiagnostics`, `SemanticModel.GetDiagnostics`, workspace diagnostic APIs, and diagnostics-with-analyzers APIs from callbacks because they can force binding or re-enter analyzer work. Code fixes should use diagnostics supplied by `CodeFixContext`.
- Treat analyzer execution as a Roslyn-like workspace service. Analyzers register narrow syntax/symbol actions and should be stateless; the workspace/analyzer driver owns traversal, caching, cancellation, and invalidation. A cold analysis can walk a document once, while later edits should rerun only actions affected by invalidated syntax or symbol scopes.
- Treat syntax-node analyzer invalidation conservatively. Existing syntax-node actions are document-scoped by default; only opt into node-scoped reuse when the diagnostic is truly local to the analyzed node and its stable semantic context. Do not make analyzer infrastructure assumptions based on current flaws in a broad analyzer such as unused-variable analysis.
- For analyzer performance isolation, project options may disable individual built-in analyzers with `RavenDisabledAnalyzers` by analyzer type name. Treat this as a diagnostic participation switch, not as a substitute for fixing broad analyzer logic.
- Favor binder-owned state over broad syntax-node caches when the state is logically scoped to that binder. External caches may decide whether a binder is still valid for a syntax tree/compilation increment, but stale binders should not self-heal.
- Use snapshot-level invalidation for binder reuse. Reuse is valid only when the syntax identity and semantic context are still equivalent: parent binder/member signature/import scope/compilation options must still match. If that context changes, recreate the binder and let its owned symbols and diagnostics be recreated lazily.
- Design changes so one-shot compilation remains authoritative, while incremental compilation can reuse valid binders and cheaply recreate invalidated binders.
- Full binding is acceptable when required for correctness, including cold language-service queries. Prefer cheap available-state paths only when they are sound, deterministic, and fall back to the normal bind path when information is missing or ambiguous.
- For language-service performance, fix the compiler API path first. The VS Code extension and LSP layer should mainly schedule, cancel, and present deterministic compiler answers; they should not suppress semantic features because a compiler answer might require binding.
- Features such as hover, inlays, completion, diagnostics, and symbol lookup should converge on the same compiler-owned semantic facts. If one path can resolve a symbol/type, the other paths should be able to obtain the same answer through public semantic APIs.
- Keep language-service request lanes distinct. Foreground requests such as hover, completion, signature help, definition, and rename may preempt or bypass stale background work. Background diagnostics, analyzers, semantic tokens, and broad inlay passes should be cancellable, versioned, and allowed to skip/requeue.
- The LSP may cache rendered presentation per document version, such as hover markdown or inlay labels, but not semantic truth. Compiler APIs own semantic state and correctness.

## Testing

Add focused coverage at the right layer:
- syntax tests for parse shape and recovery
- semantic tests for diagnostics and symbol or model behavior
- operations tests when operation shape changes
- codegen or runtime tests for observable behavior
- binder or semantic-model tests for binder-owned state, invalidation behavior, and cheap public semantic queries
- incremental edit-shape tests for recovery scenarios users hit in the editor, especially wrapping top-level statements in `func Main` by typing an opening wrapper and later adding `}`, or by creating an empty block and pasting statements into it
- language-server tests for request presentation, cancellation/scheduling, and editor-facing regressions after compiler behavior is covered

Do not add stable tests that assert emitted opcodes or exact lowered instruction sequences.
Prefer observable behavior, metadata shape, symbol shape, operation shape, and diagnostics.

If temporary emitted-instruction tests are needed during development, keep them under `test/Raven.CodeAnalysis.Tests/CodeGen/Development`.

## Validation

1. Run the baseline test split if this is the first code change in the task.
2. Build only what is necessary.
3. Run focused tests for the changed feature area.
4. Run runtime or emission-heavy tests separately if the change affects those paths.
5. Format touched files with `dotnet format ... --include`.

Use the current tool split when validating behavior:
- `rvnc` / `Raven.Compiler` is the compiler driver for one-shot compiles,
  project builds, sample builds, and MSBuild-facing behavior.
- `rvn` / `Raven` owns developer commands such as `rvn dev syntax`,
  `rvn dev dump pretty`, and `rvn dev bound-tree`.
- Do not use `rvn` as evidence that compiler-driver behavior is correct; use
  `rvnc`, the sample build scripts, or targeted compiler tests for that.

## Notes

- Keep compiler components immutable.
- Prefer diagnostics over exceptions.
- If current compiler behavior is clearly wrong for intended Raven semantics, fix the compiler rather than encoding the wrong behavior in tests.
