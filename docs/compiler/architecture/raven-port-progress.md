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
| `LinePositionSpan`, `FileLinePositionSpan` | `Text/TextCoordinateContractTests.cs`, `Text/TextValueRecordTests.cs` |
| `TextChange`, `TextChangeRange` | `Text/TextValueRecordTests.cs` |
| `TextUtils` | `Text/TextUtilsTests.cs`, including mixed line endings, UTF-16 positions and named tuple access from C# |
| `SourceText`, `TextLine`, `TextLineCollection` | Existing `Text/SourceTextTests.cs`, including edits, change chains, surrogate-aware readers, copying, lazy lines, writes and cancellation |

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
| PORT-005 | `ImmutableArray.CreateBuilder<TextLine>` correctly closes the builder's containing type but leaves member parameters/results using `T`. | Shared nested-symbol substitution defect | Include constructed containing-type substitutions when creating the nested symbol. Symbol regression failed before the correction; emitted immutable-array builder calls now execute correctly. | C# fix implemented; carry the correction and tests when the symbol layer is ported. |
| PORT-006 | The faithful snapshot reader uses string range access, which was unsupported. | Shared slicing semantics with target-owned runtime mapping | .NET selects length, range offset/length validation and substring members through an explicit contract. Tests cover stored/direct bounds, inclusivity, from-end offsets, evaluation order, invalid bounds and UTF-16 units. | Implemented in the C# compiler; carry this into the later binder/target-contract port. Native slicing remains a gap. |
| PORT-007 | An unparenthesized open-ended stored range can consume a following `return` across a newline. | Parser boundary handling; requires reduced syntax regression and clarification against continuation rules | Parenthesized stored ranges are covered by slicing tests; this does not resolve the unparenthesized parser gap. | Open follow-up. |
| PORT-008 | The snapshot port compiled, but C# `SourceText[index]` failed with CS0021 because emitted indexers lacked default-member metadata. | CLI type metadata emission | Synthesize the ordinary instance indexer's `DefaultMemberAttribute` from the selected compilation's runtime declaration. Two reduced C# consumer cases failed before the fix. | C# backend fixed; carry this into the later emission port. |
| PORT-009 | Snapshot tests compile but edits/writes and lazy lines throw `TypeLoadException` for `MemoryExtensions` and `LazyInitializer` incorrectly scoped to `System.Runtime`. | .NET final assembly normalization | Consult selected framework declarations for all normalized core-library types, extending PORT-003 beyond its collection whitelist. Both reduced metadata/runtime cases failed before the fix. | C# backend correction; carry it into the emission port. |
| PORT-010 | A field-shaped declaration in the ANSI extension crashed diagnostics because an accessor-less synthesized property had no type. Duplicate incomplete properties reproduced the same failure. | Shared source symbol recovery | Retain the declared property/indexer type as a fallback until accessors exist. Both reduced cases failed before the fix. Extension storage remains invalid by design; the regex cache lives in a separate internal helper in Raven. | C# symbol/binder correction; carry it into those later ports. |

No language workaround or test exclusion has been introduced for this slice.
Public compiler API changes belong in both codebases; no target policy or Runtime
Contract change is needed for the span invariant.

## Snapshot slice

`SourceText.rvn`, `TextLine.rvn` and `TextLineCollection.rvn` are now compiler
inputs. Both parity projects link the unchanged `SourceTextTests.cs` assertions,
and assembly-identity checks include all three types. No existing test is excluded.
The snapshot slice retains its original algorithms and .NET APIs.

The workload exposed these stabilization issues:

- `StreamReader(stream, encoding)` overload preference is fixed by PORT-004,
  with the original two-argument call retained in the draft.
- `ImmutableArray.CreateBuilder<TextLine>` nested member substitution is fixed by
  PORT-005, with symbol and emitted-runtime regressions. The original builder calls
  remain in the draft.
- `cache ??= List<int>()` is parsed but reports unsupported `??`; null-coalescing
  assignment remains a reduced compiler gap to fix with correct lazy evaluation
  and storage validation. It is no longer needed by the change-value types after
  removing their mutable empty-list caches; this is not evidence that the language
  gap itself is fixed.
- String range indexing is implemented by PORT-006. The original slicing
  expressions remain in the snapshot reader and pass the shared reader tests.
- C# indexer consumption and framework scopes are fixed by PORT-008 and PORT-009;
  all 14 existing snapshot cases now pass against the Raven implementation.

Some rejected direct spellings are deliberate language differences, not compiler
bugs: Raven parameters are immutable and ordinary null checks do not narrow
nullable storage. The draft uses mutable local copies and explicit type-pattern
bindings as specified by the parameter and nullability documentation. Assignment
expressions yield unit, so C# assignment-valued getters need explicit getter
statements. Keep these adaptations local; do not redesign the model around new
Raven idioms. The type reference `System.Text.Encoding.UTF8` is qualified to avoid
confusion with `SourceText.Encoding`.

## Next dependency slice

Complete the independent text helpers (`AnsiColor` and `AnsiExtensions`) and
extend snapshot I/O/hash coverage before moving into lexer/parser dependencies.
`SourceTextWriter` depends on syntax nodes and follows that dependency group.
Continue preserving each discovered failure as a regression at its owning layer.
Open coalescing-assignment and stored-range parser gaps remain stabilization work.

Keep C# tests as the main parity oracle. A bounded Raven translation of text tests
may be explored in its own project (author direction, 2026-11-01); do not merge it
into the compiler project or replace the C# coverage. The full test-suite port
remains a later phase, and compiler porting remains the priority.

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

## Text value records

`FileLinePositionSpan`, `TextChange` and `TextChangeRange` are now positional record
structs in both implementations. Their equality, hashing, operators and copies
use their declared value components. `TextSpan` and `LinePosition` already supply
explicit value equality and hashing; retain their validation and ordering rather
than discarding them for a shorter declaration. Record constructor parameter names
match their public properties; `TextChangeRange` keeps length-then-span order.

`NoChanges` remains an instance property returning `IReadOnlyList<T>`, but now
returns `Array.Empty<T>()`. A caller can no longer mutate the shared empty-list
cache by casting it to `ICollection<T>`. The existing custom change-value display
strings are preserved. Both line-span records explicitly define the same display
format: Raven's generic record display intentionally quotes strings and formats
nominal nested values differently, so relying on synthesized display would make
these public compiler types disagree across implementations.

The same **45 C# tests pass against each implementation** on .NET 11, including
value/boxed equality, hash collections, deconstruction, init-only copy semantics,
empty-collection immutability and shared display strings. No tests were ported.
The focused main-compiler text/location suite also passed **45 tests**, including
existing `SourceTextTests` and `LocationTests`, against the C# changes. The snapshot
implementation itself remains an unfinished Raven draft; these main-compiler
results do not claim that the Raven snapshot implementation ran.


## Nested member substitution checkpoint — 2026-11-01

The focused pre-change generic-symbol baseline passed 15 tests. A new reduced
`ImmutableArray.CreateBuilder<int>()` symbol test then failed on its `Add` parameter,
while confirming that the containing type was already closed. After carrying the
containing substitutions into the nested symbol, 27 focused substitution and
nested-type tests pass, including observable emitted builder behavior. The .NET 11
seed compiler builds successfully, and the shared C# tests pass against both text
implementations (45 each). Whitespace formatting and `git diff --check` pass.
This closes PORT-005; snapshot drafts still
await string range indexing support and are not counted as ported components.


## String slicing checkpoint — 2026-11-01

The pre-change range baseline passed 52 tests; all 13 initial string-slice cases
failed before implementation. The final focused suite passes 67 tests, including
UTF-16 and non-writable-slice coverage. The seed compiler builds on .NET 11, and
the snapshot drafts compile without changing their string slicing expressions.
No native runtime validation is claimed. Syntax and TextMate tokens are unchanged.


## Indexer interop checkpoint — 2026-11-01

The adjacent indexer/interface-member baseline passed 57 tests. Reduced class and
struct C# consumers then reproduced CS0021. After adding default-member metadata,
all 59 focused tests pass, including C# compilation and reflection checks. The
shared snapshot tests remain the end-to-end consumer workload for this correction.


## Framework scope checkpoint — 2026-11-01

Two new facade tests failed on emitted assembly scopes while the 11 adjacent
normalizer/collection tests passed. After extending selected-declaration lookup,
28 scope, normalizer, collection, span and generic-invocation tests pass. The
shared snapshot suite now passes 59 tests per implementation on .NET 11, including
edits, cancellation-aware writes and lazy line materialization. This verifies the
runtime failures from PORT-009 are resolved for the port workload.


## Snapshot acceptance checkpoint — 2026-11-01

The three snapshot sources are promoted from drafts without replacing slicing,
immutable-array builders, span-based edits or lazy initialization with workarounds.
After the owning compiler fixes, the shared C# suite passes **59 tests against
C# and 59 against Raven**, with no skipped cases. The original C# implementation
remains in place. This is a .NET 11 component checkpoint, not compiler self-hosting
or native target qualification.


## Extension recovery checkpoint — 2026-11-01

The extension suite passed 140 tests before the correction. Both reduced invalid
storage cases then reproduced the accessor-less property exception. With the
source-declared type retained, the same extension classes plus the two regressions
pass **142 tests**. The .NET seed builds, and ordinary extension lookup/generic
inference coverage remains passing. Storage is still invalid, so the ANSI port
uses an internal regex-cache helper rather than adding extension backing fields.
