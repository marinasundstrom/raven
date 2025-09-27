# AGENTS.md

## Scope
This file applies to the entire repository.

## Project Summary
Raven is an experimental compiler for a Swift/Rust/F#-inspired language targeting .NET. The compiler imitates the Roslyn compiler architecture and aims to be concept compatible with it.

Major components:

- `src/Raven.CodeAnalysis` — core compiler (syntax tree, binder, semantic model, code generation).
- `src/Raven.Compiler` — CLI entry point and sample programs.
- `test/Raven.CodeAnalysis.Tests` — unit tests.
- `tools/NodeGenerator` / `tools/BoundNodeGenerator` — code‑generation utilities for syntax nodes, bound nodes, and symbol visitors.
- `docs/` — language design notes and specification.
- Ignore the `test/Raven.CodeAnalysis.Samples.Tests` project.

## Build & Test
Run all commands from the repository root unless noted. If any test command runs for longer than two minutes without the process completing, consider it stuck:

```bash
# Generate syntax nodes (run whenever working in Raven.CodeAnalysis or when Model.xml, Tokens.xaml, and NodeKinds.xml change).
# These generator projects do not run automatically in Codex, so execute them before building the full solution.
cd src/Raven.CodeAnalysis/Syntax
dotnet run --project ../../../tools/NodeGenerator -- -f
cd ../../..

# Generate compiler diagnostics (run when DiagnosticDescriptors.xml changes).
# As above, ensure this generator runs prior to `dotnet build` so the solution sees the latest outputs.
cd src/Raven.CodeAnalysis
dotnet run --project ../../tools/DiagnosticsGenerator -- -f
cd ../..

# Build and run tests for the main project
dotnet build              # build all projects
dotnet test test/Raven.CodeAnalysis.Tests
```

If compiler errors are obscured by large numbers of warnings, temporarily lower the warning output with `dotnet build --property WarningLevel=0` to focus on the errors.

If your changes touch documentation only, you may skip `dotnet build` and `dotnet test` unless they are needed to verify an example or behavior.

## Coding Guidelines
* Use idiomatic C# style, following .NET conventions.
* All compiler components are immutable; avoid in-place mutation.
* Emit diagnostics instead of throwing exceptions whenever possible.
* Keep services loosely coupled—prefer interfaces and dependency injection.

## Contribution Checklist
* Format each changed code file using `dotnet format <path to dir of solution or project file> --include <comma separated list with file paths>` to respect `.editorconfig` rules. Do **not** run `dotnet format` solely to reformat Markdown files.
* Run `dotnet build` and `dotnet test` (omit for documentation-only changes unless verification is required).
* Ensure generated files (e.g., via `tools/NodeGenerator`) are up to date. (Required for building Raven.CodeAnalysis)
* Add or update unit tests for every fix or feature.
* Include concise commit messages (`feat:`, `fix:`, `docs:` etc.).
* Provide PR summaries referencing relevant diagnostics or examples.
* For details on diagnostics, see `docs/compiler/diagnostics.md` and `src/Raven.CodeAnalysis/DiagnosticDescriptors.xml`.
* Update the language specification, grammar, and related docs when features change.

## Additional Notes
* Workspace management and incremental compilation are key; favor additive changes.
* Consult `docs/` for language grammar and feature design before modifying syntax or semantics.
* Instruct Codex to conclude the diff instead of showing every change, and avoid gathering all line numbers for very large diffs.
* When debugging the `ravc` output assemblies (e.g., `test.dll`), leverage `ilspycmd` for inspection and analysis.
* When implementing new language features, prefer representing them through lowering when appropriate.
