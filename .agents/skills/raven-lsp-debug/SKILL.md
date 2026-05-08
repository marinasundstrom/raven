---
name: raven-lsp-debug
description: Troubleshooting workflow for Raven language service and editor failures. Use when investigating hover, completion, definition, diagnostics, or other LSP and editor integration problems. Covers required log capture, repro reduction, and cross-checking client and server behavior.
---

# Raven LSP Debugging

Use this skill for language service and editor failures.

## Required Logs

Capture both sides:

- server logs from `logs/raven-lsp.log`
- client lifecycle and request logs from the VS Code `Raven` output channel

Include relevant excerpts when reporting or fixing hover, completion, or definition failures.

## Workflow

1. Reduce the issue to a minimal source example.
2. Record the exact editor action that fails.
3. Reproduce the request with the headless language-server harness when possible:

   ```bash
   dotnet run --project tools/Raven.LanguageServer.Headless/Raven.LanguageServer.Headless.csproj /property:WarningLevel=0
   ```

   The default run opens `samples/projects/efcore-vehicle-costs/src/Api/Main.rvn` and simulates hovers for the current EF Core sample. Pass a project directory, source file, and optional target names to narrow the run:

   ```bash
   dotnet run --project tools/Raven.LanguageServer.Headless/Raven.LanguageServer.Headless.csproj /property:WarningLevel=0 -- samples/projects/efcore-vehicle-costs samples/projects/efcore-vehicle-costs/src/Api/Main.rvn UseNpgsql
   ```

   There are three useful replay modes:

   ```bash
   # Replay hover positions from logs/raven-lsp-performance.txt.
   dotnet run --project tools/Raven.LanguageServer.Headless/Raven.LanguageServer.Headless.csproj /property:WarningLevel=0 -- samples/projects/efcore-vehicle-costs samples/projects/efcore-vehicle-costs/src/Api/Main.rvn --replay-performance-report --replay-count 10

   # Hover random syntax targets in a file.
   dotnet run --project tools/Raven.LanguageServer.Headless/Raven.LanguageServer.Headless.csproj /property:WarningLevel=0 -- samples/projects/efcore-vehicle-costs samples/projects/efcore-vehicle-costs/src/Api/Main.rvn --random-hover --random-count 50 --random-seed 1729

   # Replay exact LSP positions, using zero-based line:character coordinates.
   dotnet run --project tools/Raven.LanguageServer.Headless/Raven.LanguageServer.Headless.csproj /property:WarningLevel=0 -- samples/projects/efcore-vehicle-costs samples/projects/efcore-vehicle-costs/src/Api/Main.rvn --position 15:29
   ```

   Use this harness to simulate hover behavior without VS Code, gather timings, and inspect semantic performance counters such as symbol-info fallback, type-info fallback, and bound-node bind fallback.
   For deterministic regression coverage, use the language-server unit-test hover replay helpers to load inline Raven source, open it through the mock workspace/document store, and orchestrate hovers at exact positions or marker-derived positions.
4. Check the client log to confirm the request lifecycle and parameters.
5. Check the server log to see request handling, failures, or missing symbol results.
6. Correlate the failing request with syntax, symbol lookup, binding, diagnostics behavior, compiler API caching, and language service code.
7. Add focused regression coverage if the failure is fixed in code.

## Notes

- Do not report only one side of the logs.
- If the compiler model is clearly wrong, fix the underlying compiler or language-service behavior rather than encoding client-side workarounds.
- Consider that hover, completion, definition, or diagnostics failures may be compiler-side issues exposed by the LSP, especially when incremental semantic caches or available-symbol APIs disagree with one-shot compilation.
- The language server should mostly present data provided by public compiler APIs. Keep LSP-side inference minimal unless the compiler API cannot reasonably provide the data yet.
- Keep the public compiler API Roslyn-like unless there is an intentional Raven-specific divergence. LSP, analyzers, refactorings, and completion should normally call APIs such as `GetTypeInfo`, `GetSymbolInfo`, and `GetDeclaredSymbol`, not cache-specific helper methods.
- Semantic caching is a compiler API responsibility. Public semantic APIs should decide internally whether to answer from cached or incremental state, or re-bind when necessary. Prefer fixing cache correctness, concurrency, and expensive lookup paths in `Raven.CodeAnalysis` over adding LSP workarounds.
