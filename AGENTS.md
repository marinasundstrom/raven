**Scope:** Entire repository.

**Project:** Raven is an experimental .NET compiler inspired by Swift/Rust/F#. Key folders:
- `src/Raven.CodeAnalysis` for compiler core
- `src/Raven.Compiler` for the CLI
- `test/Raven.CodeAnalysis.Tests` for unit tests
- `tools/*Generator` for codegen utilities
- `docs/` for spec and design

Ignore `test/Raven.CodeAnalysis.Samples.Tests`.

## Baseline

Before making code changes, establish a test baseline once:

```bash
scripts/test-baseline.sh
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

- Format touched files with `dotnet format <solution|project> --include <files> --no-restore`.
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
