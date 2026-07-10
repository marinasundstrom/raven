**Scope:** Entire repository.

**Project:** Raven is an experimental .NET compiler inspired by Swift/Rust/F#. Key folders:
- `src/Raven.CodeAnalysis` for compiler core
- `src/Raven.Compiler` for the `rvnc` compiler driver
- `src/Raven` for the `rvn` developer/project command tool
- `test/Raven.CodeAnalysis.Tests` for unit tests
- `tools/*Generator` for codegen utilities
- `docs/` for spec and design

Ignore `test/Raven.CodeAnalysis.Samples.Tests`.

## Baseline

Before making code changes, establish a targeted baseline once. Use
`docs/testing/test-impact-map.md` to choose the smallest relevant build and test
set for the area you are about to touch. Prefer a feature suite or focused test
filter over the full baseline when the change is scoped.

Run the full baseline when the change is broad or cross-cutting, when test
infrastructure changes, when the current failure state is unknown, or when the
impact map does not identify a smaller trustworthy baseline:

```bash
scripts/test-baseline.sh
```

For feature-scoped work, start with:

```bash
scripts/test-feature-suite.sh <suite>
dotnet test <project-file-path> --filter '<focused filter>' /property:WarningLevel=0
```

Run runtime/emission-heavy tests separately when needed:

```bash
scripts/test-runtime-isolated.sh
```

When running ad hoc tests, use `WarningLevel=0`:

```bash
dotnet test <project-file-path> /property:WarningLevel=0
dotnet test /property:WarningLevel=0
```

## Build Rules

Run the solution build script only when needed:
- first compile in a fresh workspace or after a clean checkout
- after changes to syntax or bound model inputs that drive generation, including `tools/*Generator`, syntax model definitions, bound model definitions, or generator config

```bash
scripts/codex-build.sh
```

After that, prefer targeted builds such as:

```bash
dotnet build <project path> --property WarningLevel=0
```

## Engineering Rules

- Follow idiomatic .NET style.
- Treat compiler components as immutable.
- Prefer diagnostics over exceptions.
- Keep services loosely coupled via interfaces/DI.
- Focus on incremental, additive changes.
- Review `docs/` before altering syntax or semantics.
- Fix clearly wrong compiler behavior rather than working around it in Raven code.
- If intended behavior is unclear, reduce the repro, inspect `docs/`, then fix or clarify the behavior.

## Compiler API Design

- Keep the public compiler API Roslyn-like unless there is an intentional Raven-specific divergence.
- Do not expose cache-specific or incremental-state helper APIs to analyzers, refactorings, completion, hover, or LSP code.
- Public APIs such as `GetTypeInfo`, `GetSymbolInfo`, and `GetDeclaredSymbol` decide internally whether to answer from cache, available incremental state, or re-bind when necessary.
- If language-service usage makes a public semantic API too expensive or prone to blocking, fix the compiler API implementation rather than bypassing it from the language server.
- The language server should mainly present data from `Raven.CodeAnalysis`; semantic caching and lookup correctness belong in the compiler API.

## Testing And Coverage

- Compiler bug fixes must be locked with focused tests.
- Keep generated files up to date.
- When touching a failing test area, clean it up as you go.
- Legacy emitted-instruction and lowered-shape assertions are development scaffolding, not authoritative product coverage.
- Do not add stable tests that assert emitted opcodes or specific lowered instruction sequences.
- Prefer diagnostics, metadata shape, symbol shape, operations shape, and observable runtime behavior.
- If temporary emitted-instruction tests are needed during development, keep them under `test/Raven.CodeAnalysis.Tests/CodeGen/Development`.
- Exclude development-only codegen tests from normal baseline/runtime stabilization passes.

## Contribution Requirements

- Format touched files with `dotnet format whitespace <solution|project> --include <files> --no-restore`.
  Use `dotnet format style` or `dotnet format analyzers` only when intentionally applying those fixes; analyzer/style formatters can rewrite code beyond whitespace.
- Run relevant build and test steps unless the change is docs-only.
- Update specs, grammar, and docs alongside syntax or semantic changes.
- Update `CHANGELOG.md` for behavior changes.
- For new language features, also evaluate language service support and TextMate grammar coverage.

## Use Repo Skills

Use repo skills for task-specific workflows:
- `raven-feature-workflow` for language and compiler feature work
- `raven-debug-compiler` for parser, binder, lowering, emit, and runtime investigation
- `raven-test-triage` for baseline/runtime stabilization and regression coverage
- `raven-lsp-debug` for language service failures

**External components:**
No external type-union analyzer project is part of this repository.
