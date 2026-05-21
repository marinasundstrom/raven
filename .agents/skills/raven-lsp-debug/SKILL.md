---
name: raven-lsp-debug
description: Troubleshooting workflow for Raven language service and editor failures. Use when investigating hover, completion, definition, diagnostics, inlays, semantic tokens, document symbols, request stalls, or other LSP/editor integration problems. Covers required log capture, headless repros, compiler API cross-checks, lazy binding, request scheduling, and the compiler-owned semantic model direction.
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

## Compiler Boundary

- The language server should present compiler answers. It should not own semantic invalidation, symbol inference, overload selection, binder selection, or cache policy.
- First verify the Roslyn-shaped compiler APIs: `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, diagnostics, operations, and available public semantic entry points.
- If those APIs are wrong, slow, or cache-dependent, fix `Raven.CodeAnalysis`. Keep LSP-side semantic inference temporary and remove it once the compiler can answer.
- Binders are the compiler execution units. Method binders own parameters; block binders own immediate locals, statement/expression binding state, and binder-produced diagnostics.
- Lazy binding is expected. A hover, inlay, completion, or diagnostics request may be the first path to trigger binding; once it does, later paths should observe the same compiler-owned cached symbols, types, and diagnostics.
- Prefer correctness and deterministic compiler-owned answers over cold-start speed. Cold first queries may bind; optimize later without changing semantic meaning.
- Treat available-state APIs as opportunistic fast paths. If available state is incomplete or context-sensitive, use the authoritative semantic API that can bind instead of presenting a guessed answer.
- Do not make editor features intentionally incomplete just to avoid binding. If a result is semantically required, ask the compiler for it; optimize the compiler path or request scheduling if the cold path is too slow.
- Inlays should use the same semantic model answers as hover and diagnostics. They may skip tooltips or stale background work for UX reasons, but type/parameter annotations should not disappear because a separate LSP policy refused to bind. Type annotation text should stay source-friendly; do not show fully qualified names merely as a performance shortcut.
- The LSP may coordinate request cancellation, prioritize interactive requests, skip stale background work, and avoid monopolizing semantic access, but it should not change semantic meaning.

## Request Pile-Ups

- When hovers, inlays, semantic tokens, and document symbols appear stuck, check whether a full-document/background request started first and failed to complete.
- Compare client request start/complete events with `logs/raven-lsp.log` and `logs/raven-lsp-performance.txt`.
- Use the headless harness for hover and inlay ranges to separate compiler cold-path cost from VS Code scheduling.
- When investigating hangs, compare the same project with editor inlays enabled and disabled. A project that loads without inlays but stalls with inlays points to request pressure or inlay-triggered cold semantic binding, not necessarily a broken compiler answer.
- When investigating stale or surprising diagnostics, compare compiler diagnostics without source-code analyzers against diagnostics with analyzers enabled. This keeps binder/compiler diagnostics distinct from analyzer diagnostics and avoids chasing analyzer behavior as a binder regression.
- If a request is slow because public semantic APIs force broad binding, fix the compiler path. If a request blocks newer interactive work, fix LSP scheduling/cancellation without duplicating compiler semantics.
- For inlay flicker or disappearing hints, check whether the server returned an empty result while semantic access was busy. Prefer returning cached results for the same document version and range-filtering them over clearing the editor UI.
- For slow inlays, separate three costs: semantic model materialization, binding needed for missing symbols/types, and presentation formatting such as type-name qualification. Fix broad metadata lookup or formatting costs before suppressing annotations.

## Notes

- Do not report only one side of the logs.
- If the compiler model is clearly wrong, fix the underlying compiler or language-service behavior rather than encoding client-side workarounds.
- Keep language-service and compiler documentation up to date when fixing or clarifying editor-facing behavior; if the behavior should be documented but is missing from `docs/`, consider adding it.
- Consider that hover, completion, definition, or diagnostics failures may be compiler-side issues exposed by the LSP, especially when incremental semantic caches or available-symbol APIs disagree with one-shot compilation.
- The language server should mostly present data provided by public compiler APIs. Keep LSP-side inference minimal unless the compiler API cannot reasonably provide the data yet.
- Keep the public compiler API Roslyn-like unless there is an intentional Raven-specific divergence. LSP, analyzers, refactorings, and completion should normally call APIs such as `GetTypeInfo`, `GetSymbolInfo`, and `GetDeclaredSymbol`, not cache-specific helper methods.
- Semantic caching is a compiler API responsibility. Public semantic APIs should decide internally whether to answer from cached or incremental state, or re-bind when necessary. Prefer fixing cache correctness, concurrency, and expensive lookup paths in `Raven.CodeAnalysis` over adding LSP workarounds.
