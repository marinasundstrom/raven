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
   For edit recovery regressions, use headless edit probes to model the way developers actually reshape code: type an opening wrapper such as `func Main() {`, later add the closing `}`, or create `func Main() { }` and paste existing top-level statements into the body. Compare the first semantic query after the edit with a cold one-shot compilation and with a follow-up edit inside the stabilized owner.
   For deterministic regression coverage, use the language-server unit-test hover replay helpers to load inline Raven source, open it through the mock workspace/document store, and orchestrate hovers at exact positions or marker-derived positions.
4. Check the client log to confirm the request lifecycle and parameters.
5. Check the server log to see request handling, failures, or missing symbol results.
6. Correlate the failing request with syntax, symbol lookup, binding, diagnostics behavior, compiler API caching, and language service code.
7. Add focused regression coverage if the failure is fixed in code.

When comparing editor behavior with command-line behavior, use the current
tool split: `rvnc` / `Raven.Compiler` is the compiler driver for one-shot
compile and project build repros, while `rvn` / `Raven` is for developer
commands such as syntax, pretty dump, and bound-tree views.

## Compiler Boundary

- Use `docs/compiler/architecture/live-semantic-model.md` as the shared reference
  for how compiler-owned semantic state, analyzer scheduling, diagnostic lanes,
  and LSP request prioritization should fit together.
- The language server should present compiler answers. It should not own semantic invalidation, symbol inference, overload selection, binder selection, or cache policy.
- First verify the Roslyn-shaped compiler APIs: `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, diagnostics, operations, and available public semantic entry points.
- If those APIs are wrong, slow, or cache-dependent, fix `Raven.CodeAnalysis`. Keep LSP-side semantic inference temporary and remove it once the compiler can answer.
- Binders are the compiler execution units. Method binders own parameters; block binders own immediate locals, statement/expression binding state, and binder-produced diagnostics.
- `Compilation` and `SemanticModel` are the semantic service boundary for the language server. LSP handlers should obtain the compiler-owned semantic model for the requested document snapshot and then call public semantic APIs rather than reading or composing internal cache state.
- Lazy binding is expected. A hover, inlay, completion, or diagnostics request may be the first path to trigger binding; once it does, later paths should observe the same compiler-owned cached symbols, types, and diagnostics.
- Prefer correctness and deterministic compiler-owned answers over cold-start speed. Cold first queries may bind; optimize later without changing semantic meaning.
- Treat available-state APIs as opportunistic fast paths. If available state is incomplete or context-sensitive, use the authoritative semantic API that can bind instead of presenting a guessed answer.
- Do not make editor features intentionally incomplete just to avoid binding. If a result is semantically required, ask the compiler for it; optimize the compiler path or request scheduling if the cold path is too slow.
- Inlays should use the same semantic model answers as hover and diagnostics. They may skip tooltips or stale background work for UX reasons, but type/parameter annotations should not disappear because a separate LSP policy refused to bind. Type annotation text should stay source-friendly; do not show fully qualified names merely as a performance shortcut.
- The LSP may coordinate request cancellation, prioritize interactive requests, skip stale background work, and avoid monopolizing semantic access, but it should not change semantic meaning.
- Foreground semantic requests such as hover, completion, signature help, definition, and rename should not wait behind broad background work unless they need the exact same semantic model state. Background diagnostics, analyzers, semantic tokens, and full-document inlays should be cancellable and may be skipped/requeued for a newer document version.
- LSP-side caches should be presentation-only and versioned, such as rendered hover markdown or inlay labels. Symbol/type/diagnostic truth belongs to `Raven.CodeAnalysis`.
- Cross-file edits are normal. After adding or changing another source file, a hover in the original file should resolve symbols through the current project compilation snapshot rather than stale per-document state.

## Request Pile-Ups

- When hovers, inlays, semantic tokens, and document symbols appear stuck, check whether a full-document/background request started first and failed to complete.
- Compare client request start/complete events with `logs/raven-lsp.log` and `logs/raven-lsp-performance.txt`.
- Use the headless harness for hover and inlay ranges to separate compiler cold-path cost from VS Code scheduling.
- For CPU traces, profile the built headless apphost directly rather than `dotnet run`; profiling `dotnet run` mostly captures CLI/MSBuild startup and wait frames. Use a bounded trace with rundown enabled so method symbols are preserved and the profiler stops cleanly:

  ```bash
  dotnet build tools/Raven.LanguageServer.Headless/Raven.LanguageServer.Headless.csproj --property WarningLevel=0 --no-restore
  dotnet trace collect --duration 00:00:12 --format speedscope \
    -o /tmp/raven-traces/<scenario>.nettrace \
    -- tools/Raven.LanguageServer.Headless/bin/Debug/net10.0/Raven.LanguageServer.Headless \
       <project-dir> <source-file> <target-symbol> <headless-options>
  dotnet trace report /tmp/raven-traces/<scenario>.nettrace topN --number 80
  ```

  Avoid open-ended `dotnet trace collect -- <headless-app>` for these short LSP probes; EventPipe shutdown can leave the target waiting even after the app prints its results. If Asynkron Profiler is used, run it against the built apphost as well, and guard direct collection with a watchdog if the scenario previously showed EventPipe shutdown issues. Keep the raw headless timing output alongside the trace, because short scenarios can be dominated by startup, sleeps, and idle waits in aggregate profiler views.
- When investigating hangs, compare the same project with editor inlays enabled and disabled. A project that loads without inlays but stalls with inlays points to request pressure or inlay-triggered cold semantic binding, not necessarily a broken compiler answer.
- When investigating stale or surprising diagnostics, compare compiler diagnostics without source-code analyzers against diagnostics with analyzers enabled. This keeps binder/compiler diagnostics distinct from analyzer diagnostics and avoids chasing analyzer behavior as a binder regression.
- When isolating analyzer performance, disable expensive built-in analyzers per project with `RavenDisabledAnalyzers` before changing compiler or LSP scheduling. Analyzer names may use the short analyzer type name, for example `UnusedVariableAnalyzer`.
- For analyzer diagnostic delays, inspect workspace analyzer events as well as LSP request timings. Raven's analyzer driver should behave like a Roslyn-style scheduler: one cold full document walk is acceptable, but analyzers should be registered as narrow stateless actions so later edits can invalidate and rerun only affected syntax or symbol scopes.
- If analyzer diagnostics disappear while analyzer work is skipped, canceled, or
  failed, treat that as a diagnostic-lane bug. Background analyzer failures
  should preserve the previous valid analyzer diagnostics and requeue work
  instead of publishing an empty diagnostic set.
- When a code-action request stalls, check whether a code-fix provider recomputed diagnostics or diagnostics-with-analyzers. Code fixes should use the diagnostics supplied by `CodeFixContext`; asking workspace or semantic-model diagnostic APIs from a code action can force broad binding and analyzer execution.
- If a request is slow because public semantic APIs force broad binding, fix the compiler path. If a request blocks newer interactive work, fix LSP scheduling/cancellation without duplicating compiler semantics.
- Treat structural edits that change executable ownership, such as wrapping top-level statements in `func Main`, as first-class recovery cases. Verify that changed-owner detection identifies the source-level owner and that later body-only edits can reuse unaffected semantic state.
- For inlay flicker or disappearing hints, check whether the server returned an empty result while semantic access was busy. Prefer returning cached results for the same document version and range-filtering them over clearing the editor UI.
- Treat large full-document inlay requests as background presentation work: they should prefer cached or available compiler state and avoid cold expensive binding fallbacks. Small full-document, precise, or visible-range inlay requests may trigger the authoritative compiler bind when needed.
- Do not eagerly build tooltip markdown for full-document inlay responses. Full-document responses should prioritize correct labels and source-applicable edits; focused range requests can include richer tooltip content.
- For slow inlays, separate three costs: semantic model materialization, binding needed for missing symbols/types, and presentation formatting such as type-name qualification. Fix broad metadata lookup or formatting costs before suppressing annotations.
- For cross-file responsiveness issues, test adding a new `.rvn` file that declares a symbol and immediately hovering a reference to that symbol in another document. This exposes whether project snapshot updates and background diagnostics are blocking interactive semantic queries.
- When request timings show high `gateWait` but low semantic work, fix request scheduling or gate ownership before optimizing binders.
- When timings show low gate wait but high semantic/binding work, reduce the compiler path: prefer sound binder-owned caches, narrower binding, metadata lookup shortcuts, or incremental reuse.

## Notes

- Do not report only one side of the logs.
- If the compiler model is clearly wrong, fix the underlying compiler or language-service behavior rather than encoding client-side workarounds.
- Keep language-service and compiler documentation up to date when fixing or clarifying editor-facing behavior; if the behavior should be documented but is missing from `docs/`, consider adding it.
- Consider that hover, completion, definition, or diagnostics failures may be compiler-side issues exposed by the LSP, especially when incremental semantic caches or available-symbol APIs disagree with one-shot compilation.
- The language server should mostly present data provided by public compiler APIs. Keep LSP-side inference minimal unless the compiler API cannot reasonably provide the data yet.
- Keep the public compiler API Roslyn-like unless there is an intentional Raven-specific divergence. LSP, analyzers, refactorings, and completion should normally call APIs such as `GetTypeInfo`, `GetSymbolInfo`, and `GetDeclaredSymbol`, not cache-specific helper methods.
- Semantic caching is a compiler API responsibility. Public semantic APIs should decide internally whether to answer from cached or incremental state, or re-bind when necessary. Prefer fixing cache correctness, concurrency, and expensive lookup paths in `Raven.CodeAnalysis` over adding LSP workarounds.
