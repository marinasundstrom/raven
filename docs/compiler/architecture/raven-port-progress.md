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
without using the translation as an opportunity for broad API redesign. Raven
members are public by default: omit redundant `public`, explicitly mark private
implementation details, and preserve restricted and top-level type accessibility.
Apply this convention while porting, including to the existing translated files. The
explicitly approved exception is `LinePositionSpan`: both codebases now use a
positional record struct with init-only coordinates, value equality, deconstruction
and copy/with support. C# uses `readonly record struct`; Raven uses its immutable
positional record properties. Constructor parameter names become `Start` and
`End`, matching the properties. Other bounded value-object cleanups are permitted when shared tests establish
preserved or improved semantics. `LinePosition` remains a struct for now: its
negative-coordinate validation and ordering must survive any record conversion. Raven's `while`
spells the indexed loop in `TextUtils`; it preserves the original control flow.
Raven classes are non-inheritable by default; omit C# `sealed` rather than copying
it, since Raven's `sealed` declares an abstract closed hierarchy.

A source translation that fails to express intended behavior is a compiler
workload: reduce it, classify syntax/semantic/API/emission/runtime ownership, and
fix the compiler with a focused C# regression. Do not encode compensating source
workarounds. Carry shared fixes into already-ported components, and record a
requirement when the owning component has not yet been ported.

## Component map

Raven sources preserve the corresponding C# namespace and relative directory.
Text sources correspond by basename under `src/Raven.CodeAnalysis/Text/`;
lexer input lives under `Syntax/InternalSyntax/Parser/`.

| Component | Shared C# coverage |
| --- | --- |
| `TextSpan` | Existing `Syntax/TextSpanTests.cs`; `Text/TextCoordinateContractTests.cs` |
| `TextSpanExtensions` | `Text/TextCoordinateContractTests.cs`, including direct C# static and extension calls |
| `LinePosition` | Existing `Text/LinePositionTests.cs`; `Text/TextCoordinateContractTests.cs` |
| `LinePositionSpan`, `FileLinePositionSpan` | `Text/TextCoordinateContractTests.cs`, `Text/TextValueRecordTests.cs` |
| `TextChange`, `TextChangeRange` | `Text/TextValueRecordTests.cs` |
| `TextUtils` | `Text/TextUtilsTests.cs`, including mixed line endings, UTF-16 positions and named tuple access from C# |
| `SourceText`, `TextLine`, `TextLineCollection` | Existing `Text/SourceTextTests.cs`, plus `Text/SourceTextIOTests.cs` for encoding/disposal, checksums and generic/non-generic enumeration |
| `AnsiColor`, `AnsiExtensions` | `Text/AnsiExtensionsTests.cs`, including SGR values, extended color sequences, incomplete escapes and C# static/extension calls |
| `SeekableTextSource` | `Syntax/Parser/SeekableTextSourceTests.cs`, including bounded retention, lookahead, checkpoints, absolute offsets, EOF and constructor invariants |
| `DiagnosticSeverity`, `DiagnosticDescriptor`, `LocalizableString`/literal/resource implementations, `DiagnosticInfo` | Shared `Diagnostics/DiagnosticPrimitiveTests.cs` |
| `CompilerDiagnostics` | Shared catalog lookup/cache checks and cross-assembly descriptor-field parity |
| `SyntaxFacts` | Shared string decoding, identifier escaping and keyword tests; exhaustive cross-assembly kind classification, token text, precedence and UTF-16 identifier classification |
| `SyntaxKind`, `Token` | Cross-assembly generated-kind identity check; shared `Syntax/Parser/TokenValueTests.cs` |

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
| PORT-011 | With a small buffer, lexer input trimming can drain the reader and discard unread characters or live checkpoints. | Shared `SeekableTextSource` buffer retention | Recompute the required relative offset as the buffer moves; trim only consumed history older than every saved position, releasing pinned history when checkpoints are popped. Three original regressions failed in both implementations before correction. | Corrected in both C# and Raven; shared buffering tests retain the workload. |
| PORT-012 | The port emitted its own `SyntaxKind` but token signatures referred to the C# seed's enum. | Shared import binding during source declarations | Resolve indexed source declarations before provisional metadata fallback and retain source-owned lookup results. The reduced source/reference collision failed before the fix; both file orders now preserve semantic and emitted identity. | C# binder corrected; carry into the binder port. |

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
- `cache ??= List<int>()` originally reported unsupported `??`. PORT-015 fixes
  lazy evaluation and storage validation. Removing the change-value types'
  mutable empty-list caches was a separate cleanup, not the language correction.
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

The independent text helpers and snapshot I/O/hash coverage are now included.
`SeekableTextSource`, generated syntax kinds and token values are now ported and
tested. Diagnostic payload dependencies (`LocalizableString` and its literal
implementation, `DiagnosticDescriptor`, `DiagnosticSeverity`, `DiagnosticInfo`)
are now translated, including resource-backed localization. Continue with the
remaining lexer dependencies (parse options and scanner contracts),
then the lexer itself. The diagnostic catalog is generated from the shared XML. Literal strings
now have text-based value equality; resource strings have culture-independent
identity based on their resource configuration. Both corrections are shared by
C# and Raven. `DiagnosticInfo` retains reference identity and its caller-owned
argument array rather than introducing deep equality over mutable inputs.
Keep generated syntax-kind definitions tied to their source model rather than
maintaining a divergent handwritten copy. `SourceTextWriter`
depends on syntax nodes and follows that dependency group.
Continue preserving each discovered failure as a regression at its owning layer.
The stored-range parser gap remains open; coalescing assignment is corrected by
PORT-015 below.

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


## Text helper and I/O checkpoint — 2026-11-01

`AnsiColor` and `AnsiExtensions` are compiled Raven sources alongside the original
C# files. The regex cache is on a small internal Raven helper because extension
members have no backing storage; the public extension/static call shape is
unchanged. The shared C# suite passes **73 tests per implementation**, with no
skips, covering ANSI values and stripping plus explicit/default encodings, stream
disposal, SHA-256 content hashes and both line enumeration interfaces. Assembly
identity checks include both newly ported public types. `SourceTextWriter` is the
remaining text-namespace source and is deferred only for its syntax-node dependency.


## Lexer input checkpoint — 2026-11-01

The targeted pre-change lexer baseline passed 90 tests. The first eight shared
reader cases reproduced three bounded-buffer failures in each implementation.
After the retention correction and constructor guards, the shared corpus passes
**82 tests per implementation**. The adjacent lexer, parser-recovery, trivia
checkpoint and reader selection passes **135 tests**. .NET metadata checks verify
the ported reader remains non-abstract, non-inheritable and has no public fields.
Internal test access uses narrowly named friend assemblies in each codebase; the
reader has not been made public for testing. The Raven member-accessibility
cleanup is covered by the same C# consumer suite.


## Generated tokenization boundary

The C# node generator has an explicit `--raven-syntax-kind <output>` mode that
emits the Raven enum from the same sorted model/node-kind/token definitions as
C#. This mode does not regenerate C# nodes or update their stamp. The Raven
project owns its generated file and regenerates it before compilation, including
on a clean checkout. Generated output is ignored; model and generator inputs stay
versioned. Enum numeric identities follow the existing C# generation order and
are not promised stable across model revisions.

A C# test in the Raven parity project references the seed assembly through an
explicit alias and compares every emitted enum name, value and underlying type.
This additional cross-assembly test does not substitute C# types into the Raven
implementation. The lexer `Token` definition is separated from `ILexer.cs`, and
shared C# tests cover its constructors, interned text, UTF-16/source widths,
payload values and equality. Access modifiers follow the public-by-default Raven
convention; private state remains explicit.

The node generator itself remains C# for now. Its eventual Raven translation is
a separate tool project, with output parity and clean-regeneration qualification
required before replacement.


## Source/import identity checkpoint — 2026-11-01

The existing import/namespace selection passed 67 tests before the correction.
A reduced source enum colliding with the referenced compiler's enum then failed
its symbol-identity assertion. After the correction, those cases and both file
orders pass **69 tests**. The unchanged Raven token source is the retained port
workload; no signature or naming workaround was introduced.


## Syntax-kind and token checkpoint — 2026-11-01

The pre-change syntax-facts/lexer baseline passed 117 tests. The full
`scripts/codex-build.sh` generator/build workflow passes. The C# generated enum's
SHA-256 is unchanged, and the Raven project's first build generated its initially
missing enum file. Subsequent builds reuse that output without rerunning its
generator. The shared C# corpus passes **85 tests against each implementation**;
the Raven test project also passes its **one additional** full enum/assembly
identity check (86 total). `Token` retains its existing constructors and struct
value semantics; a record/factory redesign would affect many callers and is not
part of this slice (author direction, 2026-11-01).

The broad baseline for the build/parity changes terminated with the known
RavenDoc enrichment failure recorded under PORT-015; no full-baseline pass is
claimed.

## Diagnostic dependency checkpoint — 2026-11-01

Port the diagnostic severity enum, descriptor, literal localizable strings and
internal diagnostic payload without redesigning their construction APIs.
Descriptors keep ID-only equality; both implementations now explicitly implement
`IEquatable<DiagnosticDescriptor>` and matching null-safe equality operators.
Display text and severity remain outside descriptor identity. The port keeps
reference classes and uses `open` where the C# API is non-sealed. Nullable values
use Raven's specified pattern narrowing. `Token` is unchanged.

The new six shared diagnostic cases passed against C# before translation.
The full shared corpus then passed 91 cases per implementation (92 in the Raven
project including the generated-enum identity check), covering C# calls, defaults,
expanded arguments, identity, equality and hashing. Only modern .NET is executed.

### PORT-013: exported array collectors and delegate declaration completion

The initial diagnostic translation compiled, but C# could not call
`DiagnosticInfo.Create` with omitted or expanded arguments. A reduced metadata
test confirmed that the source method was a collector but lacked
`ParamArrayAttribute`. CLI emission now obtains the attribute from the selected
references and applies it to array collectors, preserving explicit attributes.
Ordinary arrays and non-array collectors are excluded. Non-array collector export
remains an interop gap, documented in the language reference/website.

Adjacent delegate coverage exposed dropped collector flags in top-level and
nested declaration completion, followed by duplicate `Invoke` symbols on repeated
completion. Both declaration paths now normalize collectors and reuse already
owned delegate runtime members. The binder and emitter are not yet ported; these
fixes are requirements for their Raven translations. Regression coverage checks
source symbols, method/constructor/delegate metadata and runtime calls.

The focused pre-change parameter baseline passed 18 tests; the corrected
parameter/delegate/diagnostic selection passes 32. The broad baseline remains
separate and pending. The focused regression run uses a separate test output
directory to avoid changing the binaries used by ongoing baseline batches.
A website build started before the author prohibited RavenDoc generation completed;
future porting slices update documentation/website sources without generation.
Grammar is unchanged: the collector syntax already exists. Language-service
consumers receive the corrected delegate symbols through existing compiler APIs.

## Localization checkpoint — 2026-11-01

### PORT-014: resource lookup and localizable-string equality

The C# resource-backed implementation previously inherited `object.ToString()`
and never looked up the resource. New shared tests reproduced that failure and
literal wrappers' reference equality (two failures; 92 other cases passed).
Both source implementations now resolve text with `CurrentUICulture`, return an
empty string for a missing key, and preserve resource-manager exceptions.

Literal wrappers compare text and exact runtime type. Resource wrappers compare
resource key, source type, manager identity and exact runtime type. Equality and
hashes remain stable across display-culture changes. The base class explicitly
retains reference identity for custom implementations that do not override it.
Typed equality and null-safe operators delegate to the same object-equality
contract. Classes and existing constructors remain intact; a record hierarchy
rewrite would not add stabilization value here. See
[Diagnostic value contracts](../api/diagnostics.md).

Shared tests cover literal equality, nulls, hashing, inherited type boundaries,
custom implementation identity, resource configuration and culture changes.
The port now includes `LocalizableResourceString`; implementation-identity checks
ensure that the Raven tests use its Raven-produced type. No language rules or
highlighting grammar change, and no RavenDoc generation is run for this slice.

Validation: **95 shared C# tests pass against each implementation**, plus the
Raven project's one additional enum/assembly check (96 total). These runs execute
on .NET 11. The earlier broad baseline continues separately against its original
main-test binaries and is not claimed as validation of this new slice.

## PORT-015: coalescing assignment needed by the diagnostic catalog

The generated C# catalog uses lazy `??=` getters. Raven parsed the operator but
sent it through ordinary binary-operator binding, reporting unsupported `??`.
A reduced runtime selection reproduced the failure. Binding now validates the
original writable location and conversion, then guards the write with a null
pattern. Receiver/index capture is shared with mutation stabilization; arbitrary
user RHS expressions are not rewritten. Null patterns avoid overloaded equality.

Property/indexer coverage also exposed nested statement-only block emission that
stored a nonexistent `unit` value. Value-position blocks now emit `unit` when no
result expression supplies one. Coverage uses observable behavior rather than
emitted-instruction assertions. The binder and emitter have not yet been ported;
these corrections must be retained in their Raven translations.

Assignment remains unit-valued in Raven, so generated lazy getters will use a
`??=` statement followed by an explicit return. That is the documented language
translation, not an assignment-valued C# expression copied into Raven. The
language specification is also the website source; both are updated without
running RavenDoc. Existing grammar already recognizes `??=`.

The pre-change adjacent compound/coalescing baseline passed 50 tests. The earlier
broad baseline has now terminated with the previously recorded RavenDoc
`TypeRelationshipsAndEnrichmentUseSemanticContracts` failure (missing
`../Extras/indexer_Item.html`); it is not a full-baseline pass and is not rerun
as part of porting documentation generation.

Validation: 66 focused/adjacent assignment, null-coalescing, block and unit tests
pass on .NET 11. The retained C# tests remain authoritative runtime regressions;
no emitted-opcode assertions were added. The existing compiler-port parity corpus
passes 95 shared cases against each implementation, plus the Raven project's
one additional enum/assembly check (96 total).

## Generated diagnostic catalog — 2026-11-01

`DiagnosticsGenerator --raven-catalog <output>` consumes the same descriptor model
and placeholder conversion as the C# generator. It produces the internal Raven
`CompilerDiagnostics` class without modifying C# outputs or the C# generation
stamp. The port project tracks the XML, generator sources and project as inputs,
and includes the generated file even when it was absent during initial project
evaluation. Generated Raven files remain ignored by Git.

The translation retains individual lazy descriptor fields, catalog caches,
ordinal ID membership and ID-to-descriptor lookup. Raven's unit-valued assignment
requires `??=` followed by an explicit return in getters. The catalog is not
hand-maintained and the tool itself remains C# until its separate tool-port slice.
DiagnosticBag extension generation follows the bag/location dependencies rather
than introducing substitute implementations into this partial port.

Six shared C# cases check descriptors, property/lookup identity, ID sets, caching
and unknown IDs. A Raven-project-only check compares every descriptor property
against the seed assembly, including catalog order and all text/severity/default
fields. It uses the seed only as an oracle, not as the implementation under test.
Tests that touch the existing unsynchronized lazy catalog caches share an xUnit
collection so their identity checks are not affected by concurrent first access.

The pre-change C# parity selection passed 100 tests. The initial additive generator
change preserved C# catalog and bag-extension SHA-256 hashes. Field-by-field
runtime comparison then caught a C#-escaped apostrophe in XML message RAV0307:
C# consumed the backslash while Raven preserved it. The XML now contains plain
text, both generators quote/escape that text explicitly, and a shared regression
checks the intended message. C# generated spelling changes for that message,
without changing its runtime text. No language rules change and no RavenDoc
generation is run for this slice.

Validation: the required `scripts/codex-build.sh` workflow passes. The final shared
corpus passes **101 C# tests against each implementation**, with two additional
cross-assembly checks in the Raven project (103 total). All 363 descriptors match.
The first build generated an absent catalog, and unchanged-input target execution
leaves the output timestamp unchanged. The port build reports intentional `!`
assertion warnings in generated lazy getters, alongside the existing port
warnings; no errors remain. Execution validation is .NET 11 only. The broad
baseline's known RavenDoc failure remains recorded separately under PORT-015.

## String decoding and span slicing — 2026-11-01

Port the string-literal helpers in `SyntaxFacts.StringLiterals` directly, retaining
`ReadOnlySpan<char>` signatures, Unicode scalar validation, consumption rules and
out parameters. Twenty new shared C# cases exercise both implementations.

- **PORT-016 (fixed):** the original decoder matched a literal null character
  after a backslash instead of the character `0`. Both implementations now decode
  `\0` as documented. The new test failed against the original C# implementation.
- **PORT-017 (fixed in seed, retain when binding is ported):** a later enclosing
  local declaration incorrectly hid an already declared nested local. The reduced
  regression now passes without renaming locals in the Raven translation.
- **PORT-018 (fixed in seed, retain when binding/flow are ported):** span range
  indexing lacked support. The .NET target resolves `Length`, range offsets and
  `Slice`; shared binding captures receiver/range once and preserves storage.
  Escape analysis now follows ref-like calls and result blocks after visiting
  their captures, preserving stack/local-reference/scoped origins. Regression
  tests cover aliasing, evaluation counts, bounds, returned stack/scoped slices
  and valid unscoped parameter slices. No neoCLR support is inferred.

The language specification doubles as website source. Existing grammar already
recognizes these constructs; no highlighting update or RavenDoc generation is
needed. Generator tools and both output modes remain preserved.

Validation: the seed builds; 185 focused/adjacent compiler tests pass.
121 shared C# tests pass against each implementation
(123 total in the Raven test project including the two cross-assembly checks).
Execution is .NET 11 only. The previously recorded broad-baseline RavenDoc
failure is unchanged; no full-baseline success is claimed.

## Generated and handwritten syntax facts — 2026-11-01

The retained NodeGenerator now supports `--raven-syntax-facts <directory>`, emitting
token and node facts from the same XML model and classification rules used for
C#. The C# output mode remains intact. The Raven project tracks generator/model
inputs and explicitly includes outputs even when absent at project evaluation.

Port `SyntaxFacts.Identifier` and `SyntaxFacts.Operators` with the existing
character-category rules, keyword escaping, overloadable-token set and split
greater-than counts. Private state remains private and readonly. The generated
lookup tables retain dictionaries and hash sets; private construction helpers
replace C# dictionary initializers without changing lookup semantics. The token
text method retains its static `SyntaxFacts` API and .NET extension metadata.

The original C# `SyntaxFactsTests` now run against both implementations, with
additional escaping, split-token and extension-call coverage. Cross-assembly
checks compare every syntax kind's classifications/text/precedence, every token
spelling's keyword/reserved lookup, and both identifier predicates for every
UTF-16 code unit. The C# assembly is an oracle only; production port code uses
its own generated tables and helpers. No language-rule change or RavenDoc
generation is part of this slice.

### PORT-019: output discards (fixed in seed; retain when binding is ported)

The direct identifier-escaping translation uses `TryParseKeyword(..., out _)`.
The seed rejected `_` as an ordinary value. Output discards now remain untyped
binding placeholders until overload selection, then become separate temporary
storage with the selected parameter's type. They do not contribute generic
inference or conversion preference. Input-reference discards remain errors.
The Raven helper retains the direct call instead of introducing a dummy local.

Focused runtime tests cover metadata calls, generic inference from other inputs,
overloads selected by inputs and distinct storage for multiple discards. Negative
tests cover ambiguous output-only overloads and `ref`/`in` discards. The binding
placeholder is eliminated before lowering; target backends receive ordinary
address expressions. The language reference (also website source) is updated;
existing grammar already covers `out` and `_`, with no RavenDoc generation.

### PORT-020: explicit extension container metadata (fixed in seed)

C# callers could invoke the ported token-text method statically but could not use
extension syntax: explicit method attributes did not mark the containing type.
The CLI type emitter now adds the container marker for extension methods across
partial declarations, without duplicating an explicitly written attribute or
marking unrelated methods. The same path continues to mark extension declarations.
Retain this correction when the emitter is ported. Focused metadata tests cover
both container forms, ordinary-method exclusion and execution; the shared parity
suite compiles C# extension calls against the Raven implementation.

### PORT-021: canonical field initializer completion (fixed in seed)

Runtime parity found that generated keyword dictionaries initialized by factory
calls were emitted as null. A reduced static-field test showed that declaration
order itself was intact: an early binding had stored an error-recovery expression,
and full binding constructed a duplicate field that the type's member table
ignored. Typed field rebinding now completes the existing canonical symbol's
initializer, retaining references and declaration position. This is a compiler
gap, not a Raven language divergence; the generated factories remain direct.

Regression tests exercise the forward factory call with and without earlier
semantic queries and verify that an unresolved initializer still prevents emit.
Retain this change when the member binder is translated. Shared C#/.NET behavior
remains the default; intentional Raven-specific differences are distinguished
from compiler gaps, and unresolved language-design choices are raised with the
author rather than hidden in port adaptations.

Validation for this syntax-facts slice: the original syntax/identifier baseline
passed 30 tests; its expanded selection passes 48. The by-reference baseline
passed 47 tests. The combined syntax, discard, by-reference and extension-metadata
selection passes 117 tests, with 14 additional initializer/adjacent cases passing
after canonical initializer completion. Another 49 declaration-isolation, constant
binding and uninitialized-field analyzer cases pass. The overload-resolution feature suite
passes 421 tests. `scripts/codex-build.sh` passed for the generator changes, and
targeted seed builds passed after the compiler corrections. C# generated token
and node facts remain byte-for-byte unchanged; an unchanged-input Raven generation
target preserves both output timestamps. Runtime validation is .NET 11 only.
The previously recorded broad-baseline RavenDoc failure remains outstanding.

Final parity: **166 shared C# tests pass against each implementation**, plus five
cross-assembly checks in the Raven project (**171 total**). These checks compare
all syntax kinds and token spellings and all 65,536 UTF-16 code units, as well as
the existing syntax-kind and diagnostic catalog parity checks. Removing both
new generated files before project evaluation regenerates identical content and
includes each file exactly once. No C# semantic divergence was needed for these
fixes. The complete compiler port and native neoCLR validation remain unfinished.

## Conditional directive evaluation — 2026-11-01

Port `ConditionalDirectiveParser`, its private expression parser,
`ConditionalDirectiveInfo`, and `ConditionalDirectiveKind` directly to Raven.
The immutable directive metadata remains a sealed reference record. Keep the
boolean evaluation order, complete expression validation, first-error offsets,
identifier boundaries and case-sensitive symbol lookup. The lexer itself remains
unported; its `ParseOptions`/diagnostic dependencies need a separate slice.

Twenty-four shared C# cases cover valid/invalid expressions, non-short-circuited
syntax validation, record equality, deconstruction and nonmutating C# `with`.
The parity gate passes 190 tests against each implementation, plus five Raven
cross-assembly oracle checks (195 Raven tests total). The focused pre-port
conditional-directive baseline passed seven tests.

### PORT-022: record-class copy interoperability and runtime type (fixed in seed)

The translated metadata record exposed missing CLI clone metadata: C# rejected
`with` on the emitted Raven record. Synthesize the record clone contract and use
virtual copy dispatch in shared lowering. This also corrects base-typed Raven
copies that previously constructed the static record type. Keep the target-owned
metadata spelling and update both native declaration collection and portable
callable planning; shared synthesis alone did not reach those paths.

The final focused .NET selection passes 88 tests: record semantics/planning,
C# consumers, generic copies, base/derived copies, existing closed record
inheritance, adjacent `with` behavior and conditional directives. The native
source-core probe passes; its new record-copy artifact executes on the neoCLR
interpreter with result 42 and checks derived-state retention and original-state
preservation. No native JIT/AOT or full self-hosting claim is made. The owning
binder/synthesis/lowering components are not yet ported; preserve this correction
when translating them. See [record copying](../record-inheritance-equality.md)
and [the target evidence](../neoclr-cli-bridge.md).

No RavenDoc/site generation was run. The published language source now describes
runtime-type-preserving copies and .NET C# consumer support.
