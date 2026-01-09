# AGENTS.md

**Scope:** Entire repository.

**Project:** Raven is an experimental .NET compiler inspired by Swift/Rust/F#. Key folders: `src/Raven.CodeAnalysis` (compiler core), `src/Raven.Compiler` (CLI), `test/Raven.CodeAnalysis.Tests` (unit tests), `tools/*Generator` (codegen utilities), and `docs/` (spec & design). Ignore `test/Raven.CodeAnalysis.Samples.Tests`.

**Before making changes:** run the test suite once to establish a baseline.

**Before building/tests** run the solution build script only when needed:

- The first time you compile in a fresh workspace or after a clean checkout.
- Any time the syntax/bound model inputs that drive generation change (e.g., updates under `tools/*Generator`, `src/Raven.CodeAnalysis` model definitions, or generator config files).

```bash
scripts/codex-build.sh
```

*This will make sure that the essentials (Raven.CodeAnalysis) is being correctly built*

Afterwards, you might just have to run `dotnet build <project path> --property WarningLevel=0` in order to build specific projects - unless you need to rebuild the entire syntax model or bound nodes.

**Running tests with WarningLevel=0:** use one of the following forms.

```bash
dotnet test <project-file-path> /property:WarningLevel=0
dotnet test /property:WarningLevel=0
```

**Coding guidelines:** follow idiomatic .NET style; treat compiler components as immutable; prefer diagnostics over exceptions; keep services loosely coupled via interfaces/DI.

**Contribution checklist:** format code with `dotnet format <solution|project> --include <files> --no-restore`; run build/test (unless docs-only); keep generated files up to date; add/update tests; write concise commit messages; summarize PRs with relevant diagnostics; update specs/grammar/docs alongside feature changes.

**Additional notes:** focus on incremental, additive changes; review `docs/` before altering syntax/semantics; ask Codex to collapse large diffs; inspect `ravc` outputs with `ilspycmd` (install via `dotnet tool install --global ilspycmd`); prefer implementing new features via lowering where possible. Unit tests can request an `ITestOutputHelper` parameter to write diagnostics via `WriteLine`.

**External components:** `TypeUnionAnalyzer` lives in a separate project; ignore it unless explicitly instructed.
