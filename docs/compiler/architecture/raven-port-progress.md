# Raven compiler source port

The port follows [ADR-0004](decisions/0004-port-first-bootstrap.md) and the
[bootstrap procedure](bootstrap-procedure.md). C# remains maintained alongside
Raven. The initial compiler host and output target are .NET; neither self-hosting
nor neoCLR hosting is claimed by these component tests.

Work proceeds on `codex/raven-compiler-port` in the separate
`Raven-compiler-port` worktree. Commit each validated slice; keep the C# compiler
alongside the Raven sources as the maintained reference and future bootstrap seed.

## Layout and translation policy

- `src/Raven.CodeAnalysis` remains the C# compiler.
- `src/Raven.CodeAnalysis.Raven` contains the Raven translation, preserving the
  `Raven.CodeAnalysis` namespaces and public type names. Its distinct assembly
  name permits separate test consumers during the partial port. It is not yet a
  replacement compiler assembly and is not referenced by production compiler code.
- `test/CompilerParity/CSharp` and `test/CompilerParity/Raven` are C# xUnit projects.
  Both link the same test sources through `test/CompilerParity/SharedTests.props`.
  They reference the respective implementation directly; tests do not substitute
  C# types or run Raven behavior through the C# implementation.

Translate members and algorithms one-to-one where possible. Retain ordinary .NET
interfaces, exceptions, value semantics and public signatures. Use necessary Raven
syntax (`init`, `func`, `val`, `self`, interpolation and extension declarations)
without using the translation as an opportunity for broad API redesign. The
explicitly approved exception is `LinePositionSpan`: both codebases now use a
positional record struct with init-only coordinates, value equality, deconstruction
and copy/with support. C# uses `readonly record struct`; Raven uses its immutable
positional record properties. Constructor parameter names become `Start` and
`End`, matching the properties. Other bounded value-object cleanups are permitted when shared tests establish
preserved or improved semantics. `LinePosition` remains a struct for now: its
negative-coordinate validation and ordering must survive any record conversion. Raven's `while`
spells the indexed loop in `TextUtils`; it preserves the original control flow.

A source translation that fails to express intended behavior is a compiler
workload: reduce it, classify syntax/semantic/API/emission/runtime ownership, and
fix the compiler with a focused C# regression. Do not encode compensating source
workarounds. Carry shared fixes into already-ported components, and record a
requirement when the owning component has not yet been ported.

## Component map

Each Raven file under `Text/` corresponds to the C# file of the same basename
under `src/Raven.CodeAnalysis/Text/`.

| Component | Shared C# coverage |
| --- | --- |
| `TextSpan` | Existing `Syntax/TextSpanTests.cs`; `Text/TextCoordinateContractTests.cs` |
| `TextSpanExtensions` | `Text/TextCoordinateContractTests.cs`, including direct C# static and extension calls |
| `LinePosition` | Existing `Text/LinePositionTests.cs`; `Text/TextCoordinateContractTests.cs` |
| `LinePositionSpan`, `FileLinePositionSpan` | `Text/TextCoordinateContractTests.cs` |
| `TextUtils` | `Text/TextUtilsTests.cs`, including mixed line endings, UTF-16 positions and named tuple access from C# |

The original tests retain their assertions. Added behavioral tests are shared by
both implementations and remain in the main compiler test project too. Shared
traits describe target-independent contracts. The parity projects currently run
on .NET 11 only; their implementation identity test is explicitly .NET-specific.
Implementation identity is recorded in assembly metadata, project/test-result
names and a runtime assertion about the assembly defining the ported types.

## Running parity

From the repository root:

```sh
scripts/test-compiler-parity.sh
```

The script builds the C# compiler from the current checkout, with
`UseRavenCoreReference=false` to avoid requiring a Raven-built Core to build the
seed. It passes that exact repository compiler path to the Raven project and uses
the repository's `Raven.Language.targets`, not an installed Raven SDK. This first
component set needs only .NET APIs and uses `RavenEmitCoreTypesOnly=true`.

Results and provenance are written under `artifacts/compiler-parity/` (override
with `RAVEN_PARITY_RESULTS_DIR`). The record includes checkout revision, dirty
paths, SDK version and compiler/implementation hashes, with separate TRX results.
These are local development evidence, not immutable bootstrap seed artifacts; a
qualified milestone must retain its source diff and complete dependency closure
as described in the bootstrap procedure.

## Porting ledger

| ID | Reproduction and expected contract | Classification / owner | Corrections and regression | Status |
| --- | --- | --- | --- | --- |
| PORT-001 | `new TextSpan(int.MaxValue, 1)` wrapped `End` negative. A valid span must have nonnegative coordinates and an `End` representable by `int`. | Shared existing API defect; `TextSpan` constructor | Reject `length > int.MaxValue - start` in both C# and Raven. Shared tests reject three overflowing inputs and accept maximum valid ends. Both implementations failed the three new rejection cases before the fix. | Fixed in both sources; parity validation recorded below. |
| PORT-002 | The faithful overflow guard's `int.MaxValue` reported RAV0500. Concrete-type lookup must select the public constant, excluding explicit interface implementations. | Existing CLI metadata-import defect; PE method/property/event symbols | Retain constructed generic interface names and include static members when resolving explicit implementations. Five reduced C# tests failed before the fix; the unchanged Raven guard is the retained port workload. | C# import fix implemented; metadata import has not yet been ported, so carry this correction and its regressions when that component is translated. |
| PORT-003 | A public `List<int>` result from the Raven library caused C# CS0012 requiring `System.Private.CoreLib`. Public APIs must be consumable with the selected framework reference assemblies. | Existing .NET emitted-type-scope defect; final PE normalization | Resolve retained collection/monitor type scopes from selected semantic declarations, distinguishing `System.Collections` owners from `System.Runtime` interfaces. Retain a C# consumer compilation and runtime collection test plus the unchanged `TextUtils` port. | C# backend correction implemented; carry it when the backend is ported. |
| PORT-004 | `StreamReader(stream, encoding)` tied with a longer overload needing defaults. Equivalent supplied parameters should favor an overload needing no defaults, without overriding better conversions or ranking two candidates that both need defaults. | Shared overload-resolution defect | Added the no-omission tie-breaker in C# and seven reduced runtime/diagnostic tests. Five failed before the fix; the original snapshot draft retains its two-argument call. | C# fix implemented; carry it into the later overload-resolver port. |

No language workaround or test exclusion has been introduced for this slice.
Public compiler API changes belong in both codebases; no target policy or Runtime
Contract change is needed for the span invariant.

## In-progress snapshot slice

The five `Text/*.rvn.pending` files are visible translation drafts, intentionally
not compiler inputs and not counted as ported components or passing coverage.
They will become `.rvn` sources as the second slice is made buildable, then link
`SourceTextTests.cs` into both test projects. No existing test has been excluded.

Initial analysis exposed these issues to reduce and address before accepting it:

- `StreamReader(stream, encoding)` is ambiguous against a longer optional-argument
  overload; overload preference needs a reduced regression.
- `ImmutableArray.CreateBuilder<TextLine>` leaves the nested builder's `T`
  unsubstituted; metadata generic substitution needs a regression.
- `cache ??= List<int>()` is parsed but reports unsupported `??`; null-coalescing
  assignment needs correct lazy evaluation, storage validation and regression
  coverage before using it in the port.
- String range indexing is not implemented (the current range spec describes
  arrays and user indexers); add and test the intended string slicing contract
  rather than silently changing the source algorithm to compensate.

Some rejected direct spellings are deliberate language differences, not compiler
bugs: Raven parameters are immutable and ordinary null checks do not narrow
nullable storage. The draft uses mutable local copies and explicit type-pattern
bindings as specified by the parameter and nullability documentation. Assignment
expressions yield unit, so C# assignment-valued getters need explicit getter
statements. Keep these adaptations local; do not redesign the model around new
Raven idioms. The type reference `System.Text.Encoding.UTF8` is qualified to avoid
confusion with `SourceText.Encoding`.

## Next dependency slice

Port `TextChange` and `TextChangeRange`, then `SourceText`, `TextLine` and
`TextLineCollection`, linking the existing `SourceTextTests.cs`. This adds snapshot
edits, change ranges, Unicode, encoding, stream, hash, collection, weak-reference
and lazy-initialization workloads. Keep each failure as a regression at its owning
layer before moving on to lexer/parser components.

## First component checkpoint — 2026-11-01

Starting source revision: `a1f2c8588` (including main's native concrete static
interface helper change). Validation used SDK `11.0.100-rc.3.26522.148` on macOS
ARM64, targeting .NET 11:

- Shared parity harness: **41 passed in C#, 41 passed in Raven**, no failures or
  skipped cases. This includes an implementation-identity check in each process
  and the approved `LinePositionSpan` record semantics.
- Focused text and PE-symbol/import tests: **90 passed**.
- Collection consumer, assembly-reference normalization and generic emission:
  **16 passed**, including C# compilation against framework reference assemblies
  and runtime collection behavior.
- C# seed compiler build passed. Whitespace formatting and `git diff --check`
  completed; the formatter reported workspace-load warnings. Raven's explicit
  member visibility produces redundant-`public` style warnings; no compiler error
  is suppressed.

The broad baseline started before production compiler changes and stopped on
`RavenDocGenerationTests.TypeRelationshipsAndEnrichmentUseSemanticContracts`:
the receiver page did not contain `../Extras/indexer_Item.html`. The main test
binary used by that run was preserved while focused post-change tests used a
separate output directory. This baseline failure needs separate investigation;
the focused results are not a completed broad baseline or self-hosting gate.
No tests have been ported to Raven and no C# compiler component has been removed.

## Separate-worktree validation

The new `Raven-compiler-port` worktree uses SDK `11.0.100-rc.1.26425.128` from
`/usr/local/share/dotnet` (the original checkout used the separately installed
rc.3 SDK). Its required fresh-workspace `scripts/codex-build.sh` completed,
building the compiler for .NET 10 and .NET 11, Core and Macros. The original
41-case-per-implementation parity checkpoint also passed in this worktree.
The optional-overload correction passed its seven regressions plus nine existing
resolver tests (**16 total**). The overload-resolution feature suite then passed **421 tests**
with no failures or skips. The C# seed fix is committed separately from the next
text-value-record slice.
