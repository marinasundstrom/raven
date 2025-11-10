# AGENTS.md

**Scope:** Entire repository.

**Project:** Raven is an experimental .NET compiler inspired by Swift/Rust/F#. Key folders: `src/Raven.CodeAnalysis` (compiler core), `src/Raven.Compiler` (CLI), `test/Raven.CodeAnalysis.Tests` (unit tests), `tools/*Generator` (codegen utilities), and `docs/` (spec & design). Ignore `test/Raven.CodeAnalysis.Samples.Tests`.

**Before making changes:** run the test suite once to establish a baseline.

**Before building/tests** (run from repo root; the generators must run before `dotnet build`/`dotnet test` so the projects compile):

```bash
# Refresh generated code once before `dotnet build`
(cd src/Raven.CodeAnalysis/Syntax && dotnet run --project ../../../tools/NodeGenerator -- -f)
(cd src/Raven.CodeAnalysis      && dotnet run --project ../../tools/BoundNodeGenerator -- -f)
(cd src/Raven.CodeAnalysis      && dotnet run --project ../../tools/DiagnosticsGenerator -- -f)

# Build and test
dotnet build --property WarningLevel=0
dotnet test test/Raven.CodeAnalysis.Tests
```

If documentation-only changes don’t need verification, you may skip build/test. Use `dotnet build --property WarningLevel=0` when warnings hide errors.

**Coding guidelines:** follow idiomatic .NET style; treat compiler components as immutable; prefer diagnostics over exceptions; keep services loosely coupled via interfaces/DI.

**Contribution checklist:** format code with `dotnet format <solution|project> --include <files> --no-restore`; run build/test (unless docs-only); keep generated files up to date; add/update tests; write concise commit messages; summarize PRs with relevant diagnostics; update specs/grammar/docs alongside feature changes.

**Additional notes:** focus on incremental, additive changes; review `docs/` before altering syntax/semantics; ask Codex to collapse large diffs; inspect `ravc` outputs with `ilspycmd` (install via `dotnet tool install --global ilspycmd`); prefer implementing new features via lowering where possible. Unit tests can request an `ITestOutputHelper` parameter to write diagnostics via `WriteLine`.

**External components:** `TypeUnionAnalyzer` lives in a separate project; ignore it unless explicitly instructed.
