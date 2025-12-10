# Parser hardening and hang prevention

## Current recovery surface
* `BaseParseContext.SkipUntil` preserves unrecognized tokens as `SkippedTokensTrivia` before resynchronizing at an expected token or end-of-file. This keeps the token stream moving while retaining trivia for later diagnostics.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/BaseParseContext.cs†L453-L495】
* `SyntaxParser` threads skipped tokens into leading trivia on statement terminators or adds them to pending trivia when abandoning a construct, letting the parse continue even after local failures.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/SyntaxParser.cs†L305-L343】

## Logic-focused action points
1. **Guarantee forward progress per loop**: add guards that assert token position advances inside parsing loops; if no progress is detected, forcibly consume one token with `SkippedTokensTrivia` and emit a diagnostic to avoid infinite iterations.
2. **Synchronized recovery sets**: define recovery token sets per non-terminal (e.g., statement/decl/expr) and reuse `SkipUntil` with those sets instead of ad-hoc checks. This centralizes resynchronization and reduces chances of getting stuck waiting for an absent terminator.
3. **Cap skipped-token batches**: limit the number of tokens accumulated in a single `SkippedTokensTrivia` block and flush periodically to prevent large trivia lists that slow subsequent parsing and binding.
4. **Missing-node insertion**: prefer constructing placeholder nodes when required tokens are absent (paired delimiters, statement headers), then resume parsing after a bounded skip, ensuring the tree stays well-formed for later stages.
5. **Newline/brace heuristics**: when optional delimiters are missing, fall back to newline- or indentation-based recovery (e.g., treat blank line as statement end) to keep parsing progressing through blocks.
6. **SkippedTokens-aware diagnostics**: surface diagnostics that highlight skipped ranges and suggest likely fixes; ensure structured trivia survives into later phases so IDE layers can render recoverable errors without halting the parser.

## Infrastructure-focused action points
1. **Cancellation and timeout plumbing**: thread a `CancellationToken` through parser entry points and compilation drivers, and add optional wall-clock timeouts to break out of hung parses during CI or IDE sessions.
2. **Parser progress watchdog**: add a debug-only watchdog that records the last consumed token index/time and triggers a failure (or aggressive skip) if no progress occurs after N iterations, capturing contextual traces for debugging.
3. **Stress and fuzz harnesses**: integrate grammar fuzzing and large-random-file stress tests into CI, asserting that parsing completes within bounded time and that the tree remains non-null even with many `SkippedTokens` blocks.
4. **Trace logging hooks**: provide opt-in verbose tracing around critical parse loops and `SkipUntil` calls, with counters for skipped tokens and recovery hits to spot hotspots that could hang.
5. **Benchmark + heap guardrails**: add benchmarks that parse pathological inputs (deep nesting, unmatched delimiters) while measuring allocations from `SkippedTokensTrivia`; flag regressions that correlate with hangs or excessive memory use.
6. **Crash dumps for hangs**: when watchdogs/CI detect a hang, automatically capture stack traces (e.g., `dotnet-trace`/`dotnet-dump`) and store them as artifacts to speed up root-cause analysis.

## Progress
* Parser entry points now accept cancellation and timeout inputs. `SyntaxTree.ParseText` links caller tokens with an optional wall-clock timeout so hung parses terminate promptly, and the CLI exposes `--parse-timeout` to enforce the same budget when invoking `ravenc` from CI or editor integrations.
