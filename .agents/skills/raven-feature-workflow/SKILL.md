---
name: raven-feature-workflow
description: End-to-end workflow for Raven language and compiler feature work. Use when adding or modifying syntax, parsing, binding, lowering, code generation, operations, language service support, or feature documentation. Covers binder-owned semantic state, Roslyn-like compiler APIs, incremental compilation, generator rebuild decisions, docs/spec sync, changelog updates, and focused test coverage.
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
- Grammar, spec, and docs: update `docs/` for the final supported behavior.
- Changelog: update `CHANGELOG.md` for user-visible behavior changes.

## Compiler Architecture Direction

- Keep public semantic APIs Roslyn-like unless Raven intentionally diverges: callers should ask `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, diagnostics, operations, etc.
- Treat binders as execution units. A binder owns the derived semantic state for the syntax/scope it binds, such as method parameters, local declarations, labels, pattern variables, and binder-produced diagnostics.
- Keep semantic caching and incremental reuse inside `Raven.CodeAnalysis`. The language server, analyzers, completion, and refactorings should not depend on cache-specific helper APIs or choose invalidation policy.
- Favor binder-owned state over broad syntax-node caches when the state is logically scoped to that binder. External caches may decide whether a binder is still valid for a syntax tree/compilation increment, but stale binders should not self-heal.
- Design changes so one-shot compilation remains authoritative, while incremental compilation can reuse valid binders and cheaply recreate invalidated binders.
- For language-service performance, fix the compiler API path first. The VS Code extension and LSP layer should mainly schedule, cancel, and present deterministic compiler answers.

## Testing

Add focused coverage at the right layer:
- syntax tests for parse shape and recovery
- semantic tests for diagnostics and symbol or model behavior
- operations tests when operation shape changes
- codegen or runtime tests for observable behavior
- binder or semantic-model tests for binder-owned state, invalidation behavior, and cheap public semantic queries
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

## Notes

- Keep compiler components immutable.
- Prefer diagnostics over exceptions.
- If current compiler behavior is clearly wrong for intended Raven semantics, fix the compiler rather than encoding the wrong behavior in tests.
