---
name: raven-feature-workflow
description: End-to-end workflow for Raven language and compiler feature work. Use when adding or modifying syntax, parsing, binding, lowering, code generation, operations, language service support, or feature documentation. Covers generator rebuild decisions, required compiler layers, docs/spec sync, changelog updates, and focused test coverage.
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

## Testing

Add focused coverage at the right layer:
- syntax tests for parse shape and recovery
- semantic tests for diagnostics and symbol or model behavior
- operations tests when operation shape changes
- codegen or runtime tests for observable behavior

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
