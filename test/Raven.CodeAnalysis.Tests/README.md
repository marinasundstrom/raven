# Raven.CodeAnalysis.Tests Status

This is a living status board for test stability.  
Policy for contributors: when you touch a failing area, clean it up in the same change by fixing outdated tests or replacing obsolete tests with valid coverage.

## Stability Table (as of 2026-02-07)

| Area | Scope / Filter | Status | Current signal | Cleanup direction |
|---|---|---|---|---|
| Syntax parser core | `FullyQualifiedName~Raven.CodeAnalysis.Syntax.Parser.Tests` | Mostly stable | 171 pass / 8 fail in latest targeted run; failures concentrated in newline/terminator recovery expectations | Keep hardening parser recovery; rewrite stale newline/terminator assertions to match current skipped-trivia placement |
| Syntax parser incremental updates | `IncrementalSyntaxTreeUpdatesTest` | Unstable | Stack overflow during `NormalizeWhitespace` path in parser test run | Isolate and fix recursion in whitespace/trivia normalization pipeline |
| Syntax nodes/tokens/trivia | `FullyQualifiedName~Raven.CodeAnalysis.Syntax.Tests` | Unstable | Stack overflow seen in `SyntaxTokenTest.AddTrailingTriviaToToken` (and related structured trivia paths) | Fix token trivia rewrite recursion before broad syntax suite enablement |
| Legacy syntax expectations | specific stale tests | In progress | Updated: enum underlying type now `PredefinedTypeSyntax`; qualified record pattern expectation updated from member pattern | Continue auditing old syntax assertions and align with current grammar behavior |
| Full test project | `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj` | Unstable | Many unrelated semantic/symbol failures; run can end with OOM | Keep using focused filters while parser/syntax cleanup is in progress |
| Semantic assertion drift | semantic/symbol tests with text assertions | Needs ongoing updates | Multiple failures are caused by expected strings no longer matching current symbol/diagnostic display output (for example `ToDisplayString()` changes) | Update expected assertion text to current canonical display format when behavior is still correct; only treat as regression when semantic meaning changed |

## Working rule

For every parser/syntax bug fix:
1. Add or update a focused regression test.
2. If a nearby existing test is outdated, fix it in the same PR.
3. If behavior was intentionally removed, delete the obsolete test and add a replacement that documents current behavior.

For semantic/symbol test failures:
1. Verify whether failure is representational (`ToDisplayString()`, diagnostic message text, symbol formatting) vs behavioral.
2. If representational only, update expected assertion values to the new canonical output.
3. If behavioral, add/adjust coverage and fix compiler behavior before changing expected values.
