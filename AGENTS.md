# AGENTS.md

## Scope
This file applies to the entire repository.

## Project Summary
Raven is an experimental compiler for a Swift/Rust/F#-inspired language targeting .NET.  
Major components:
- `src/Raven.CodeAnalysis` — core compiler (syntax tree, binder, semantic model, code generation).
- `src/Raven.Compiler` — CLI entry point and sample programs.
- `test/Raven.CodeAnalysis.Tests` — unit tests.
- `tools/NodeGenerator` / `tools/Generator` — code‑generation utilities for syntax nodes and Roslyn-based generators.
- `docs/` — language design notes and specification.

## Build & Test
Run all commands from the repository root unless noted:

```bash
# To generate the syntax nodes (first time and then when Model.xml has changed)
cd src/Raven.CodeAnalysis/Syntax
dotnet run --project ../../../tools/NodeGenerator -- -f
cd ../../..

# Build and run tests for the main project
dotnet build              # build all projects
dotnet test               # run unit tests
```

## Coding Guidelines
* Use idiomatic C# style, following .NET conventions.
* All compiler components are immutable; avoid in-place mutation.
* Emit diagnostics instead of throwing exceptions whenever possible.
* Keep services loosely coupled—prefer interfaces and dependency injection.

## Contribution Checklist
* Run `dotnet build` and `dotnet test`.
* Ensure generated files (e.g., via `tools/NodeGenerator`) are up to date.
* Include concise commit messages (`feat:`, `fix:`, `docs:` etc.).
* Provide PR summaries referencing relevant diagnostics or examples.

## Additional Notes
* Workspace management and incremental compilation are key; favor additive changes.
* Consult `docs/` for language grammar and feature design before modifying syntax or semantics.