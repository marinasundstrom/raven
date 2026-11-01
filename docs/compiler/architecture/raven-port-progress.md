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

The independent generator support port lives in `tools/NodesShared.Raven`, beside
`tools/NodesShared`. Its C# parity projects are under `test/GeneratorParity` and
run with `scripts/test-generator-parity.sh`; they remain separate from compiler
component parity.

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
| `SyntaxKind`, `Token`, `GraphemeLiteralValue`, `EncodedStringLiteralValue` | Cross-assembly generated-kind identity check; shared `Syntax/Parser/TokenValueTests.cs` |
| `Lexer`, `LexerFlags`, `ILexer`, `IMacroBodyScanner` | Shared `Syntax/Parser/LexerTests.cs`, including literals, lookahead, diagnostics and conditional checkpoint restoration |

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

The lexer core now passes the shared C# tests in both implementations. Next,
port the remaining parse-options dependencies and syntax-node/token/trivia
infrastructure, preserving generator support for both C# and Raven output.
The current node generator emits Raven syntax kinds and facts; generated node
support must be extended before the parser can depend on Raven syntax trees.
Follow with `ParseContext`, `BaseParseContext`, and parser checkpoints.

Once those foundations pass parity, port coherent parser families in larger
chunks, keeping the translation close to C# and linking existing parser tests
against both implementations. Continue reducing and fixing language/compiler
gaps at their owning layer rather than compensating in translated parser code.
Syntax additions should follow Raven's existing conventions; the array allocation
spelling remains open to later review.

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

## Syntax annotations and lexer contracts — 2026-11-01

Translate `SyntaxAnnotation`, `ILexer` and `IMacroBodyScanner` into Raven. These
are real dependencies of syntax trees and the lexer, not substitutes for their
unported implementations. The lexer still depends on `ParseOptions`, whose
`Diagnostic`/`Location`/syntax-tree/symbol graph is not yet ported.

Keep annotations as sealed reference objects with explicit value equality,
`IEquatable<T>`, null-safe equality operators and consistent hash codes. Their
existing immutability and equality contract already meet the porting directive;
a record conversion is unnecessary. Retain the public readonly elastic marker,
private debugger helper, constructor null validation and unchecked hash arithmetic.
The lexer contracts retain their callback property, token return types, optional
lookahead index and `out bool` macro-termination parameter.

### PORT-023: same-type constructor delegation (implemented in seed)

The direct annotation translation exposed the missing `: self(...)` form.
Implement it rather than consolidating the constructor overloads. Both annotation
implementations retain their original one- and two-argument signatures.

A distinct `SelfConstructorInitializerSyntax` preserves API/syntax identity.
Resolve overloads after declarations, diagnose cyclic delegation and reject
instance access in initializer arguments. Both emission paths call the selected
constructor on the existing receiver; only the terminal constructor executes
instance initializers. Value-type receivers require their address on .NET.
`GetSymbolInfo` resolves initializer targets through the semantic model.
The existing generators retain both C# and Raven outputs from the shared models.

The target audit found no separate annotation or lexer-interface implementation
in the native backend/target contracts. Both targets use these shared compiler
objects; the native constructor/cast paths are validated separately below. Tests run
on .NET and do not establish neoCLR execution of the Raven-ported component.

### PORT-024: nullable reference `as` and native emission (fixed in seed)

The annotation's `Equals(object?)` exposed an incorrect rejection of nullable
reference operands in `as`. A reduced .NET semantic regression reproduced the failure; the native
source-core integration probe covers the actual neoCLR configuration. Compare underlying reference types when
binding the test, while retaining its nullable result. Keep the cast in the port.

The native audit also found no portable lowering for `BoundAsExpression`.
Add the missing type-test/conditional-reference-conversion path, retaining the
operand once and producing null when it is null or incompatible. The source-core
probe exercises null, mismatched and matching reference cases alongside record
copying. The .NET backend already emits its runtime safe type test.

Raven's documented default nullability policy does not narrow original storage
through `is not null && ...`; use the equivalent typed pattern binding in both
annotation implementations. This is a documented Raven distinction, not the
nullable-cast defect. No language rule or grammar change is introduced.

The native metadata builder also needed an integration correction: it admitted
only direct-base constructor calls. neoCLR commit `012474be0` on
`codex/raven-constructor-delegation` adds same-type class/struct/generic authoring
and retains exactly-once flow validation and invalid-owner rejection. The existing
local runtime already supports this chaining. The native source-core consumer
returns 42 on the interpreter, including class, struct and generic delegation,
initializer side effects, record copies and nullable safe casts. Native JIT/AOT
was not executed.

The annotation/lexer slice's shared gate passes **199 C# tests per implementation**
and five Raven cross-assembly checks (**204 Raven tests total**). Syntax ownership
baseline: 26 passed. Constructor/initializer baseline: 300 passed. The combined
post-change selection passed 368 of 370 initially; the two analyzer failures used
stale syntax-kind constants in an unrecompiled analyzer assembly. The follow-up
analyzer result and final build are recorded below. TextMate already classifies
`self`; grammar and website language-source docs now describe constructor delegation.
No RavenDoc/site generation is performed; the required generator/build script is
run with documentation MSBuild properties disabled.

Final verification: all seven constructor-naming analyzer/code-fix checks pass
after rebuilding `Raven.Analyzers`; the two initial failures are resolved.
`scripts/codex-build.sh` completes successfully with documentation disabled.
Focused metadata constructor-delegation, constructed-base and generic-base checks
pass in the separate neoCLR worktree. Both source worktrees retain their own
branches; no merge or push is performed for this slice.


### Lexer core checkpoint — 2026-11-01

The full lexer core, LexerFlags and GraphemeLiteralValue are translated alongside
the C# implementation. Existing lexer tests are linked into both parity projects.
The C# ParseOptions constructor remains; a dependency-independent configuration
constructor allows the core to be ported before that dependency graph. Explicit
private members, constructor delegation, local functions and record value shapes
are retained, with normal Raven indentation.

A shared regression reproduced an existing checkpoint defect: scanning past
`#else` and rewinding restored the character position but left the conditional
branch inactive. Both lexer sources now remember conditional state when creating
a checkpoint and restore it on rewind. The regression originally failed with
DisabledTextTrivia instead of LineFeedToken.

After the compiler corrections recorded in PORT-025 through PORT-030, the full
parity command passes **298 C# tests and 303 Raven tests**. The latter includes
five Raven-specific implementation checks. Tests cover ordinary and multiline
strings, interpolation, numeric/character literals, Unicode, lookahead and
conditional rewind. GraphemeLiteralValue retains record equality and ToString.
Validation ran on .NET 11; shared frontend behavior does not establish neoCLR
runtime execution. The Raven build still reports analyzer warnings (403 in this
run), so this is behavioral parity, not a warning-free-build claim. No RavenDoc
or website generation was run.

### PORT-025: Runtime-length array allocation

The lexer requires C# `new Token[count]`. Raven now represents this as
`Token[](count)`, an invocation of an array type with a dedicated bound allocation
and `IArrayCreationOperation`. The shared portable backend uses its existing
NewArray instruction; the .NET backend allocates default-initialized storage.
The syntax is shared, not neoCLR-exclusive. `Array<Token>(count: 10)` is a future
neoCLR design consideration, not implemented syntax or a requirement for this
port. No RavenDoc or website generation is run; language-reference source feeds
the existing website. Existing punctuation/type highlighting applies without a
new keyword. Hover/type/operation consumers use the ordinary semantic APIs.

The pre-change array/collection selection passes 118 tests. Five new syntax and
runtime tests failed before implementation and pass after the initial change.
Constant counts retain fixed-length source types, consistent with array literals;
runtime counts retain `T[]`. Portable local storage erases only this source length
annotation to vector storage; callable signature admission still rejects fixed
lengths without metadata support. The native source-core interpreter returns 42,
including allocation, zero/default values, mutation and count side effects.
The required generator/compiler build passes with documentation generation
disabled. Runtime .NET tests cover generic elements, nulls and negative lengths.
Rust-like repetition is deferred: semicolons currently separate collection
elements. Future syntax should follow Raven's existing conventions.

Final array allocation validation: **150 focused and adjacent tests pass**.
Native source-core emission passes and the interpreter returns `Int32(42)`.

### PORT-026: Shared binding corrections exposed by the lexer

Reduced C# regression tests initially reject both nested record declarations in
field signatures, a range-valued `if` branch, unsigned operations with positive
signed constants, and a forwarded `out` parameter in the left operand of `&&`.
These are shared compiler defects, not reasons to rewrite the Raven translation.

Type/member binders now resolve nested types in their containing type. Value
blocks retain their final bound expression even when that expression itself uses
a block to capture a receiver and range. Numeric binding admits representable
`int` constants with `uint`/`ulong` operands and nonnegative `long` constants with
`ulong`, while leaving shift counts and nonconstant signed operands unchanged.
This follows the ordinary [.NET/C# constant-expression conversions](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/conversions#10211-implicit-constant-expression-conversions).
Definite assignment accounts for the left operand of short-circuit expressions,
without treating the conditionally evaluated right operand as always executed.

The reduced cases plus adjacent suites pass 197 tests. A second focused run
including emitted runtime values and negative unsigned cases passes 97 tests
(the selections overlap). The native source-core fixture includes UInt64 and
validates unsigned constant operands and forwarded `out` storage; its interpreter
returns 42. Native string-range execution remains unsupported by the source-core
fixture and was not claimed; native JIT/AOT was not executed. These binder owners
remain C# until their dependency-ordered Raven translations are ready.

The broader baseline stopped after 73 passes and one failure in
`PublicDelegateWithInternalSignatureTypes_ReportsRAV0501`. Rebuilding with all
five pre-slice binder files reproduces the same missing diagnostics; the files
were then restored and the seed rebuilt. This pre-existing baseline failure
remains open; the complete baseline is not claimed green.

The full lexer also exposed literal patterns failing at newline boundaries before
another member declaration. Pattern parsing now accepts ordinary newline,
semicolon and end-of-file boundaries, including newline tokens and trivia.
The full lexer is still an unvalidated draft, including a remaining from-end
indexer gap and an uninitialized character accumulator.

Final PORT-026 selection: **240 focused and adjacent tests pass**. The complete
lexer draft now parses without syntax diagnostics; Raven emission/parity remains
pending on the remaining porting gaps.

### Lexer character decoding preparation

Character literal decoding now lives in a private helper in the C# lexer and
its Raven draft. Each loop iteration starts with the decoded character or rune;
a null result signals an already-diagnosed incomplete escape. This preserves
the existing payload distinction between `char`, `Rune`, and
`GraphemeLiteralValue` without an uninitialized accumulator or redundant initial
boxing. It does not introduce a language rule or target-specific behavior.

The focused C# lexer/literal/directive selection passes 143 tests, including
nine payload and invalid/incomplete escape regressions. The Raven draft parses
without syntax diagnostics; full implementation parity remains pending. The
next compiler gap is implicit from-end indexing on `List<T>` through its
`Count` property and integer indexer, needed by the conditional-directive stack.

The draft explicitly suppresses nullable access at the two character-accumulator
uses guarded by the incomplete-escape check. This follows Raven's documented
static nullable types: an early return after `is null` does not narrow a mutable
local. Unlike a compiler workaround, this is an intentional source-language
difference from C# nullable flow analysis (see
[Handling a nullable value](../../lang/spec/type-system.md#handling-a-nullable-value)).

### PORT-027: Implicit Index access and captured locations

The lexer keeps its directly translated `List<ConditionalFrame>[^1]` reads and
writes. Ordinary overload resolution retains priority; the shared binder falls
back to an integer indexer with accessible `Length`/`Count` and Index.GetOffset.
Bound location captures preserve evaluation order and struct storage while
retaining semantic-model indexer identity. Mutation lowering uses one offset
across getter, right operand and setter. No syntax or highlighting change is
needed. .NET remains the default target.

The pre-change indexer baseline passed 85 tests. The completed indexer/operation
selection passes **144 tests**, including explicit overload priority, reads,
simple/compound/increment writes, class and struct receiver state, evaluation
order, inherited interface Count, Index parameters, public semantic-model facts
and portable lowering. The full generator/build script passed with documentation
generation disabled. The lexer draft now compiles with zero errors; full shared
lexer parity is a separate pending gate. The earlier delegate-accessibility
baseline failure recorded in PORT-026 remains open.

Portable body lowering now admits compiler-generated managed-reference locals
under the existing capability, preserving captured storage on both targets.
neoCLR's experimental metadata writer needed matching local signature admission,
CLI projection and native-reader support, committed separately as `ed3bcf4c2`
on `codex/raven-constructor-delegation`. Its focused managed-reference tests pass.
Both the alias-mutation fixture and Raven's source-core struct-indexer probe
execute with result `Int32(42)` in the native interpreter. JIT/AOT were not
validated. The probe supplies a small positive-index System.Index fixture,
not a production implementation or evidence of a complete native Index API.

The next parity attempt passes 302 C# implementation tests, then fails to compile
the C# tests against Raven: GraphemeLiteralValue exposes malformed required-member
metadata (CS9037), and the linked LexerTests file includes two syntax-layer tests
whose SyntaxTree/SyntaxFactory dependencies are not yet ported. Fix the metadata
rather than changing the record representation; separate the syntax-layer tests
from lexer parity coverage. Lexer runtime parity is not yet established.


### PORT-028: Required-member metadata interoperability

The C# consumer failure exposed missing declaring-type RequiredMember metadata.
Source types now advertise required-member lists alongside their marked fields
and properties. A reduced required-property consumer reproduced CS9037 before
the correction. C# consumers now accept positional records, record structs,
friend-accessible records and ordinary required properties; omitted required
initializers still produce CS9035. Import also fixes the inverted/wrong-name
SetsRequiredMembers check, reproduced with an unrelated Obsolete attribute.

The record/object-creation baseline passed 158 tests. The completed selection
passes **267 focused and adjacent tests**, including lexer and syntax test
relocation coverage. Native source-core emission passes; required-member native
import remains the documented separate gap. The two lexer-file cases that use
SyntaxTree/SyntaxFactory moved to IdentifierConstructionTests in the main test
project, retaining their coverage without pulling unported syntax APIs into
lexer parity. The shared C# parity project passes 298 tests; Raven runtime parity
remains pending until its rebuilt implementation is exercised.

### PORT-029: Emission owns body binding and validation

Lexer parity exposed a computed property that returned false after workspace
analysis, disabling ordinary source text. A reduced `List<Frame>` short-circuit
property reproduced the failure through `rvnc` and a workspace test. Workspace
diagnostics came from a separate analysis compilation; passing them to Emit
skipped binding and validation of the emitting compilation itself.

Emit now collects its own diagnostics before any backend, merges supplied
analyzer diagnostics without duplicates, and rejects source errors before
writing output. This is a shared compiler boundary, with no new target policy.
The focused baseline passed 42 tests and failed the new computed-property
regression. The corrected selection passes **61 tests**, covering observable
.NET execution, source-error rejection, diagnostic preservation and target
configuration checks. The reduced `rvnc` output also contains the correct
short-circuit property. These checks do not claim native runtime execution.


### PORT-030: Local functions capture the enclosing instance

After PORT-029, lexer parity reached 295 passes with eight failures in multiline
strings and interpolation. The failures reduced to local functions accessing
instance state: explicit `self` was rejected, while implicit access could emit
invalid receiver loads. Local functions now use the enclosing lexical instance
in binding, as lambdas do, and capture analysis records that receiver. Static
containing methods and explicitly static local functions remain boundaries.

The baseline passed 24 adjacent tests and failed all four new receiver cases.
The corrected selection passes **35 tests**, including .NET execution for fields,
properties, calls and explicit `self`, semantic capture reporting, and static
boundary diagnostics. This is shared binding behavior; the binder is not yet
ported, so retain the correction when translating it. Native closure/backend
coverage remains separate from these .NET runtime checks.

### PORT-031: Public XML framework scopes

The generator serializer's Raven translation compiled, but its C# consumer
reported CS0012 for the public XmlWriter parameter because emitted metadata
referenced System.Private.Xml. Reduced XmlWriter, XmlReader and XDocument
consumer tests all failed before the correction; two existing facade tests
passed. Final .NET metadata normalization now resolves private runtime scopes
through selected framework declarations. The focused and adjacent metadata,
collection interop, core-selection and required-member selection passes **32
tests**. This is a .NET emission correction; no native XML API support is claimed.
Carry this fix when the metadata normalization component is ported.


### PORT-032: Generator model and XML serializer

The syntax-node generator's seven shared model types and XML serializer now have
Raven translations in a separate tools library. Mutable model objects retain
reference identity, open class behavior and independent collection instances.
Private helper methods remain private. The production node generator continues
using the maintained C# library; node-emitter logic is not yet ported.

A shared regression found that serialization dropped separately stored
`ElementType` values and did not normalize inline `SeparatedList<T>` forms.
Both versions now retain those values, so changing a collection element type
changes the serialized generator input used for hashing. The targeted baseline
passed nine tests and failed three collection-shape cases. Final parity passes
**14 tests per implementation**, including every collection slot in the repository
syntax model, metadata flags, escaping, defaults, public surface and assembly
identity. The required generator/compiler build passes. Regenerated Raven syntax
kinds and facts match the existing outputs. Validation uses .NET 11; the C# tools
library retains its .NET 10 target. No native runtime validation is claimed.

The translation also exposed existing, intentional nullability differences:
XName's implicit conversion declares a nullable result with NotNullIfNotNull;
Raven does not apply that flow refinement. Both implementations now call
`XName.Get` explicitly. Nullable comparison uses `EqualityComparer<T?>`, and
values guarded against null are explicitly suppressed at the XAttribute object
boundary. These are bounded API choices, not new language semantics. The Raven
build reports 15 warnings, including default-initialized mutable properties and
explicit suppression; behavioral parity does not claim a warning-free build.

The broad baseline was rerun and remains at **73 passing tests and the known
PublicDelegateWithInternalSignatureTypes_ReportsRAV0501 failure**. That unrelated
accessibility defect is still open. Source documentation was updated without
running RavenDoc or website generation.


### PORT-033: Captured local-function parameter positions

The direct node-emitter translation exposed ArrayTypeMismatchException in local
helpers that capture a StringBuilder and accept string arguments. CLI method
signatures included the hidden closure argument but parameter metadata still
started at the original position. Four reduced cases failed before the correction:
value/optional arguments raised runtime errors and ref/out updates targeted the
wrong storage. The emitter now defines the hidden parameter before authored
parameter metadata. No generator-source workaround was introduced.

The focused baseline passed 28 existing cases and failed all four regressions.
The corrected closure/receiver/semantic-capture selection passes **39 tests**.
This validates .NET execution, not native neoCLR; the portable backend owns its
own parameter layout. Carry this correction when MethodGenerator is ported.

### PORT-034: Syntax-kind, node-fact and token emitters

A separate `tools/NodeGenerator.Raven` library now translates SyntaxKindGenerator,
NodeFactsGenerator and all four TokenGenerator entry points. Shared C# tests run
against each implementation. An additional cross-implementation test compares all
eight generated source outputs using the repository XML inputs, covering both C#
and Raven output modes. The production generator still uses C#; its driver,
loaders, structural node emitters, visitors and rewriters remain to be ported.

TokenKindModel and NodeKindModel use primary-constructor records in both source
languages. Raven intentionally requires record instance data in the primary
constructor. Required names/types are constructor arguments, optional properties
retain their defaults, and value equality and init-only properties are preserved.
This does not change the lexer's Token type. Private helpers remain explicitly
private and captured local functions retain their original structure; PORT-033
fixes the compiler defect exposed by that direct translation.

The existing generator baseline passed **14 tests per implementation**. Expanded
parity now passes **21 C# tests and 22 Raven tests** on .NET 11, including exact
source-output equality. The required generator/compiler build also passes. The
Raven emitter build reports 16 warnings, including explicit null suppression and
unused builder return values. No native neoCLR execution is claimed. RavenDoc and
website generation were not run. This slice establishes more generator support
for subsequent coherent parser/syntax ports; it does not yet port the parser.


### PORT-035: Iterator callback capture storage

The visitor-generator translation exposed a missing-local emission failure when
an iterator's filtering lambda captured its loop variable. Four reduced .NET
cases reproduced failures for loop/local/parameter captures and callback mutation.
The shared iterator lowerer now retains source storage mappings and leaves nested
callable bodies independent. CLI emission reuses persistent state-machine closure
planning from async methods, redirecting captured locals to the same storage used
by callbacks across yields. The focused and adjacent iterator/async selection
passes **42 tests**. These components remain in C# and must retain the correction
when ported. Native neoCLR capture environments retain their existing limitations;
no native execution or complete iterator/callback lifetime parity is claimed.

### PORT-036: Visitor and rewriter generator

`VisitorGenerator` now has a direct Raven translation in the separate generator
library, retaining Roslyn syntax construction, iterator dispatch grouping, named
tuples, inheritance traversal and private helpers. It emits the existing C#
visitor, generic visitor and rewriter forms for green and red syntax nodes. The
production generator remains C#; structural node emitters and orchestration are
still outstanding.

Shared C# tests cover transitive dispatch ordering, empty abstract groups,
explicit kind arguments, nullable child slots and token/list traversal. The
cross-implementation test compares all six outputs against C# on the repository
XML model, including its inheritance and slot information. PORT-035 records the
iterator-capture defect found through this translation.

A second mismatch was a constructor/import collision: `List<ArgumentSyntax>()`
selected the imported Roslyn `SyntaxFactory.List<T>()`, yielding an immutable
syntax list whose ignored Add results lost the generated arguments. The Raven
translation explicitly constructs `System.Collections.Generic.List<ArgumentSyntax>()`.
C#'s `new` syntax distinguishes these targets; Raven uses ordinary call syntax,
so this qualification is required without changing generator logic. The parity
cases catch the difference. No new language syntax or lookup rule was introduced.

The pre-change generator baseline passed **21 C# / 22 Raven tests**. Final parity
passes **26 C# / 29 Raven tests** on .NET 11, including exact generated-source
comparisons. The required generator/compiler build passes. The Raven generator
library reports 26 warnings, including explicit nullable suppression and ignored
builder return values; this is not a warning-free build. No native neoCLR runtime
validation or RavenDoc/website generation was performed. Next dependency slice:
the green-node emitter, then red nodes and factories.

### PORT-037: Green-node generator inference gaps

The green-node translation exposed three binder defects through Roslyn metadata:
conditional branches found an abstract common base and then rejected it; a
collection-builder factory with a non-params ReadOnlySpan parameter received
individual arguments instead of one span; and callback shape filtering did not
repeat ordinary-argument pre-inference after leaving one generic overload.
Four reduced interop cases failed before these corrections; the adjacent
conditional baseline passed 13 tests. The corrected focused selection passes
**25 tests**, including source-defined abstract base inference, empty/single/multi
span-builder execution, incompatible-element rejection and existing immutable
collection-builder coverage.

These are shared binder corrections, with framework interop validated on modern
.NET. They do not add native span/runtime capabilities or change .NET as the
default. The owning compiler components are still C# and must retain the fixes
when ported. Language-reference sources used by the website and runtime/bridge
documentation were updated without running RavenDoc or website generation.

The generator migration has two stages: first compile the translated Raven
implementation and establish output parity; then add full Raven syntax-node,
factory, visitor and rewriter output while retaining C# output for bootstrapping.
Existing Raven syntax-kind/fact generation is not full syntax-tree generation.

The broader overload-resolution feature suite also passes **421 tests** after
these fixes. This validation is separate from generator output parity.

### PORT-038: Green-node and factory generator

The separate Raven generator library now includes a direct translation of the
complete GreenNodeGenerator: typed/raw constructors, ordered slot accessors,
abstract/inherited properties, update and With methods, visitor dispatch,
red-node creation and factory overloads. Both source versions scope the typed
constructor local to its branch and declare the factory return nullable because
abstract models intentionally produce no factory. No generated-language syntax
changed.

The generator baseline passed **26 C# / 29 Raven tests**. Expanded parity passes
**29 C# / 33 Raven tests** on .NET 11. It compares exact node and factory output
for all **256 models**, including **26 abstract nodes**, alongside focused shared
C# tests for slot ordering, inherited accessors, explicit kinds, visitor hooks,
update identity and nullable factory overloads. PORT-037 records compiler defects
found and fixed without generator workarounds. The Raven generator build reports
27 warnings; its shared-model dependency reports 15. Native execution is not
claimed.

This completes another C#-output component of the Raven-written generator.
Red-node generation, model loading and CLI orchestration remain to be translated.
Full Raven syntax-tree output follows validated translation, while C# generation
remains available for the bootstrap compiler.

The required generator/compiler build passes. Reference/website source docs were
updated; RavenDoc and website generation were not run.

### PORT-039: Negated conditional and keyword macro disambiguation

The red-node generator translation exposed `if !condition` being recognized as
an `if` macro invocation. Keyword aliases now require an adjacent bang; spaces
and comments retain ordinary keyword syntax. Existing `if!(...)` aliases still
parse. This shared parser correction applies before .NET/neoCLR backend choice;
there is no target encoding or external runtime change. The parser is not yet
ported, so no duplicate Raven implementation needs updating at this stage.

The targeted baseline passed 50 macro/conditional tests. Two reduced negated-if
cases failed before the fix while the adjacent keyword macro passed. All 53
focused tests pass after the correction on modern .NET. Language reference,
website source and grammar notes are updated; no RavenDoc/site generation ran.
Semantic editor classification consumes the corrected parser. TextMate remains
a lexical approximation rather than the authority for macro recognition.

### PORT-040: Captured for-variable initialization

The translated red-node emitter compiled but four parity tests failed at runtime:
loops assigned an IL local while captured body reads used an uninitialized
closure field. CLI StatementGenerator now initializes that field before each
body execution for array, span, range and enumerator loops. The Raven translation
retains its ordinary loops and callbacks. StatementGenerator has not yet been
ported, so its future translation must carry this correction.

The focused baseline passed 32 tests. Six reduced runtime regressions failed
before the fix (default numeric values or null references); all 38 focused and
adjacent tests pass afterward on modern .NET. Native portable lowering was
inspected and uses local storage for supported immutable captures rather than
this CLI display class. No native run or complete retained-callback lifetime
parity is claimed. Runtime Contract and neoCLR bridge notes record ownership.

### PORT-041: Red-node and factory generator

The Raven generator library now translates RedNodeGenerator alongside the C#
bootstrap source. It retains red-node constructors, child/list backing storage,
token wrappers, slot dispatch, abstract closed hierarchies, Update/With methods,
visitor calls, automatic factory defaults and XML-defined factory overloads.
Aliases and documentation trivia remain part of the generated C# output.
Abstract-node factory returns are nullable in both sources.

Imported Roslyn `Attribute` and `List` factory calls need qualification where
.NET types have the same names; this is the same construction/call distinction
already recorded for the earlier generator slices. PORT-039 records the shared
parser correction exposed by a negated conditional in this translation.

Full Raven syntax-tree output remains a later step, after the translated
C#-output implementation is validated. The production driver and XML loaders
still use C#. C# generation remains available for bootstrapping.

Follow-up: the combined generator translation takes several minutes to compile.
A read-only stack sample during this workload found binder import lookup and
metadata symbol identity hashing; this is a lead for profiling, not a proven
single cause or a reason to rewrite generator algorithms during translation.

Validation: the initial generator baseline passed **29 C# / 33 Raven tests**.
Expanded suites pass **33 C# / 40 Raven tests** on .NET 11, including exact output
comparisons for all **256 node models**, automatic and XML-defined factories,
closed hierarchies, alias fallback text and XML escaping. Four runtime failures
in the first compiled translation led to PORT-040; the unchanged loop algorithms
now pass. The final Raven generator build reports 39 warnings, plus 15 in its
shared-model dependency. The required generator/compiler build passes with both
PORT-039 and PORT-040 included. No neoCLR runtime, RavenDoc generation or website
generation was run.

### PORT-042: Nullable conditional target preservation

The translated token loader compiled but failed at runtime with an invalid CLI
stack merge for an integer/null conditional in a record property initializer.
Reduced typed-local and return cases also lost the selected integer value.
BindIfExpressionCore had unwrapped the nullable contextual target before binding
its branches. It now retains that target and applies ordinary branch conversions;
no emission-specific workaround or loader algorithm rewrite is needed.

The focused baseline passed 19 conditional/return tests. All 23 focused tests
pass after the change, including nullable returns, locals, initializers and the
TryParse/out-variable shape. Another 123 adjacent nullable-type, target-typed
expression and function-inference tests pass. Validation is on modern .NET. Native portable
lowering consumes the corrected shared bound tree and retains existing nullable
capability restrictions; no native execution is claimed. The unported binder
must carry this correction when translated. Language-reference/website source,
Runtime Contract and neoCLR bridge documentation record the behavior and scope.

### PORT-043: NodeGenerator XML input loaders

The five production XML loader functions are extracted from the C# top-level
driver into an internal SyntaxModelLoader, then translated directly beside it in
NodeGenerator.Raven. The C# driver imports those same methods. This removes
fixture-only duplicate parsing: shared C# emitter tests now exercise each
implementation's production loaders. An independent cross-implementation check
compares complete loaded node, hierarchy, factory, token and kind data from the
repository XML, rather than copying one implementation's inputs into the other.

The translation retains order, duplicates, required-attribute exceptions,
optional factory files, nullable metadata, explicit factory defaults and tolerant
boolean/precedence parsing. The private boolean helper remains explicitly private
in Raven. Mutable model identity and existing record kind equality are unchanged.
No language rule, target capability or generated-source format changes.

The production driver, validation and incremental-generation orchestration remain
in C#. Full Raven syntax-tree output follows validation of the translated tools;
C# output remains available for the bootstrap compiler.

The initial Raven compile rediscovered the documented XML nullability boundary
from the shared-model serializer slice: string-to-XName implicit conversion has a nullable result refined
by NotNullIfNotNull in C#, which Raven does not currently apply. Both loader
implementations now use XName.Get explicitly, as the serializer already does.
This is an aligned .NET API choice in both sources, not a new nullable-conversion
rule or a claim that flow-attribute refinement has been implemented.

Author direction: commence Raven syntax-tree output when its dependencies are
ready. Validated emitters can gain Raven output before the entire CLI has been
ported. Start with a compilable node hierarchy and its authored Raven runtime
foundations, then validate generated behavior with shared C# tests. Do not count
text emission alone as a working Raven syntax-tree implementation, and preserve
C# output for the bootstrap codebase.

Validation: baseline **33 C# / 40 Raven tests**; final **48 C# / 56 Raven tests**
on .NET 11. The loader suite covers metadata/defaults, ordering, missing files,
required-attribute errors and precedence parsing. Independent repository-model
comparison and all existing 256-node/factory output comparisons pass. The first
runtime run exposed PORT-042; the translated loader algorithm is retained after
the shared binder correction. An isolated loader build and its 15 shared tests
also pass. The full Raven generator reports 40 warnings, plus 15 in NodesShared;
the required generator/compiler build passes with the fix included. No native
neoCLR execution, RavenDoc generation or website generation was performed.


### PORT-044: Raven green-node output and compiler stabilization

Add matching RavenGreenNodeGenerator APIs to the C# and Raven generator codebases.
They emit green nodes and factories from the existing models without replacing the
production C# generator path. Output retains typed/raw constructors, ordered slots,
abstract/inherited properties, visitor dispatch, red creation and identity checks.
Helpers are explicitly private in Raven. Explicit parent-type aliases avoid
collisions between red and green node names.

Shared C# tests parse emitted Raven for all 256 repository models, compare exact
Raven node/factory output and compile/run a generated hierarchy against a focused
runtime-contract fixture. The production runtime port, Raven red/visitor/rewriter
output and CLI output selection remain subsequent slices.

The compile/run probe drove fixes in generic override matching, overridden method
candidate suppression, covariant-return binding/emission, nominal nullable-reference
constraints and constructor-initializer target typing. Focused tests protect each
behavior. Existing absent-slot behavior is now explicit in both generator output
paths and the C# runtime through nullable slot-array/GetSlot annotations. The binder
and emitter have not been ported yet; their translations must carry the fixes.
See [the readiness investigation](../../investigations/raven-syntax-output-readiness.md).

Pre-change baselines: **48 C# / 56 Raven generator tests**, **14 override tests**,
**41 constraint tests**, and **23 constructor-initializer tests**. Modern .NET is the
execution target. Native neoCLR covariance remains an explicit capability/metadata
gap rather than a permanent language exclusion. Reference/website source docs,
Runtime Contract and CLI bridge documentation are updated.


Compiler validation: **34 focused constructor/override checks** and **169 adjacent
semantic, generic, constraint and green-node checks** pass on .NET 11. The required
generator/compiler build passes. The full baseline stops in its first 74-test batch
with the previously recorded
AccessibilityDiagnosticsTests.PublicDelegateWithInternalSignatureTypes_ReportsRAV0501
failure (missing delegate-signature accessibility diagnostics); 73 tests in that
batch pass. This is not a claim of a clean full baseline. RavenDoc/site generation
was not invoked; project builds did emit intermediate Raven.Core documentation
through the compiler's project settings despite documentation-suppression properties.


Final generator validation: **50 C# / 58 Raven tests** pass, including exact output
parity for all repository models and compiling/executing each emitter's generated
hierarchy. The translated tool builds with 42 warnings, plus 15 in NodesShared.
An additional **17 target-capability/target-typing tests** pass, including shared
override binding under .NET and neoCLR selection with host references. These
semantic-only fixtures do not establish native core admission or native execution.
Compiler stabilization is committed separately as `5c9de9d5c`; this output slice
retains the C# driver and leaves production runtime/red/visitor output next.


### PORT-045: Stabilize reused expression-property override getters

The fresh pre-visitor baseline exposed an intermittent runtime failure in PORT-044:
the C# generator suite failed to load SampleSyntax because get_Keyword did not
implement the inherited abstract getter, while the Raven suite initially passed.
Repeated isolated runs reproduced the failure, and both suites were subsequently
observed failing. Inspection showed a nonvirtual emitted getter despite an
`override` property declaration. Temporary assembly/source capture was removed
before committing; no emitted-instruction assertions were added.

Expression-bodied property binding reused its canonical getter without refreshing
modifiers, unlike block-bodied accessor binding. It now refreshes static and
virtual/override/sealed/abstract flags while retaining identity. The shared generated
hierarchy test now covers diagnostics-first and derived-property-query-first orders,
asserts getter identity and virtual/override symbol shape, then executes the tree.
This is a shared C# binder correction to retain when that component is translated.
The generator source and generated Raven algorithms remain unchanged.

Validation: targeted compiler build passes; **51 C# / 59 Raven generator tests** and
**49 adjacent property/override compiler tests** pass on .NET 11. The prior broader
baseline's delegate-accessibility failure remains separately recorded; this scoped
fix does not claim a clean full baseline or native neoCLR execution. Visitor output
remains the next generator dependency; this runtime gap was fixed before extending
it. No RavenDoc or website generation was invoked for this slice.

The two generated-hierarchy query-order cases also pass in three fresh test-host
runs per implementation (12 case executions across six processes), addressing the
intermittent failure observed before the fix.


### PORT-046: Restore delegate signature diagnostics on member reuse

While validating the visitor-output slice, the previously recorded baseline
failure was reduced to the early member-reuse return in both delegate declaration
paths. Existing Invoke/constructor symbols prevented the current declaration
binder from receiving signature accessibility diagnostics. Reuse now validates
the existing Invoke signature into that binder's bag without replacing members.

The original public-delegate regression now passes. Four added query-order tests
cover top-level/nested public delegates, declaration queries before diagnostics,
repeated diagnostic reads and stable Invoke/constructor identity. The focused
baseline was **29 passing / 1 failing**; after the correction, **44 accessibility,
delegate semantic and delegate runtime tests pass** on .NET 11. Shared semantics
and .NET defaults are unchanged. The unported binder/semantic-model implementations
must retain this fix. Broader baseline results are recorded separately as they
complete; the focused result is not a claim that the whole baseline is green.


### PORT-047: Align primary-constructor attribute coverage with required metadata

The broader baseline passed its first four batches (354 tests), then stopped on
one record case in a 102-test batch. The type correctly carried synthesized
RequiredMemberAttribute, but the constructor-target test asserted an entirely
empty type attribute list. The assertion now excludes the constructor marker
specifically, while retaining exact constructor marker/value and diagnostic checks.
No compiler or language behavior changed. All **24 AttributeBindingTests pass**
on .NET 11. This is targeted coverage cleanup, not a full-baseline success claim.


### PORT-048: Generate Raven visitor partial declarations

Both NodeGenerator implementations now provide RavenVisitorGenerator for generic
and non-generic visitor source in red/public or green/internal namespaces. The
translation keeps the existing C# emitter's ordinal transitive-descendant dispatch
and omits abstract roots without concrete descendants. Authored runtime partials
own DefaultVisit, traversal entry points and token/list handling. Production CLI
selection and existing C# output remain unchanged.

Shared C# tests parse repository-wide output in both namespaces, compare exact
output from both emitter implementations, and exercise a three-level generated
green hierarchy. Runtime checks cover direct Accept, abstract root/intermediate
dispatch, unknown-node fallback and inherited concrete DefaultVisit behavior in
both diagnostics-first and declaration-query-first orders. Red visitor validation
is parse/parity only; the production syntax runtime and red-node/rewriter Raven
output remain future slices. No new language rule or target-specific encoding is
introduced, and no native neoCLR execution is claimed.

Validation: **57 C# / 65 Raven generator tests pass** on .NET 11, including generated
visitor execution. The required `scripts/codex-build.sh` succeeds. The first full
translated-generator build exited without diagnostics; a confirmed terminal retry
using an immutable compiler-host snapshot succeeded (42 warnings, zero errors).
No warning identifies the new visitor emitter. The cause of the first exit is not
established. Whitespace formatting and diff checks completed.

The broader baseline rerun has passed the formerly failing constructor-attribute
batch and remains in progress; this slice does not claim a green full baseline.
No RavenDoc/site generation was explicitly invoked. The required build/baseline
project graph still emits intermediate Raven.Core documentation from its project
settings despite the existing suppression wrapper.


### PORT-049: Preserve green rewriter list behavior

Reviewing the authored runtime for Raven rewriter output found two production
C# defects: list tokens used the generic result-visitor hook rather than the
concrete token hook, and every list rewrite allocated a new list. Focused tests
reproduced both (**10 passed / 2 failed** before the correction).

List rewriting now dispatches nodes, tokens, trivia and nested lists through their
appropriate hooks, allocates only after a replacement/removal, omits removed nodes,
and preserves list diagnostics/annotations and order. Default trivia rewriting
preserves the trivia. Tests cover unchanged tree identity, token-list hook dispatch,
replacement/removal, metadata retention and nested-list/trivia identity. The
production authored runtime has not yet been translated; retain these corrections
when porting it. The generated Raven runtime-contract fixture exercises the same
identity, replacement and removal principles. These are shared syntax semantics,
not target runtime changes; .NET defaults remain unchanged.

Validation: the focused runtime tests pass (**14 tests**); the combined visitor,
syntax-list, annotation and green-diagnostic suite passes **27 tests** on .NET 11.
The targeted compiler build passes. The broader baseline remains in progress;
these results do not establish a fully bootstrapped compiler or native execution.


### PORT-050: Generate Raven syntax rewriter partial methods

Both generator implementations now emit Raven rewriters over the generated
visitors. They preserve ordinal transitive abstract dispatch, return unknown nodes
unchanged independently of DefaultVisit overrides, preserve explicit kinds, route
slots through the matching token/list/node hooks, and call Update for identity.
Optional slots skip absent children; required green lists assert the authored
nullable VisitList result is present. A compile probe caught the missing assertion
before it was corrected in both emitters; compiler nullability was not weakened.

Shared C# tests parse all repository red/green output and compare exact output from
both implementations. The generated green fixture covers generic and typed child
slots, list and token replacement, node removal, list-element deletion, unchanged
tree identity and unknown-node fallback under both semantic query orders. The
production authored runtime, Raven red-node output and CLI output selection remain
separate work. Existing C# output is retained. No language-rule change or native
neoCLR execution is claimed.


Validation of this output exposed an intermittent compiler failure: an override on
DropLeafRewriter was rejected after an earlier lookup marked the partially declared
base rewriter method nonvirtual. Temporary tracing confirmed that lookup had run
before base visitor signatures existed. FindOverrideCandidate now invokes the
existing compiler-owned member-signature completion path before testing candidates.
The tracing was removed. Reduced partial generic query-order coverage and the
shared generated-runtime fixture retain the regression; the C# binder is not yet
ported and its future translation must retain the fix. No public API or target
encoding changed.

Validation: **61 C# / 69 Raven generator tests pass** with the corrected compiler.
Four additional full C# generator-suite processes pass (244 test executions),
checking the previously intermittent hierarchy failure. **86 adjacent override and
partial-type tests pass**, including runtime coverage. The translated generator
build succeeds (42 existing warnings, zero errors), the required build script
succeeds on retry, and the final targeted compiler build succeeds. Formatting and
diff checks pass. The first required-build invocation stopped without a compiler
diagnostic; no cause is claimed.

The broader baseline subsequently stopped at four metadata documentation tests
because the temporary no-doc wrapper suppressed ordinary C# XML sidecars as well
as RavenDoc output. Restoring normal C# XML output and narrowing the wrapper fixed
the fixture prerequisite: **all 20 MetadataReferenceResolutionTests pass** without
RavenDoc/site generation. No compiler/test expectation was weakened. A fresh full
baseline on the completed slice remains required; prior mixed-worktree batches are
not a final all-green baseline claim.


### PORT-051: Stabilize nullable generic bounds and generic null patterns

The red-node output probe exposed two compiler gaps. A constrained child-cache
cast from SyntaxNode to T failed for `where T: SyntaxNode?`, and generic null
patterns attempted to use Nullable<T>.HasValue on a generic parameter builder.

Nominal base/interface queries now unwrap nullable reference annotations on bounds
(and the queried base), retaining nullable value-type identity and unrelated-cast
rejection. The initial reduced baseline was **11 passing / 2 failing tests**.
The test matrix also covers nullable source casts to prevent regressing the green
slot helper. CLI generic null tests now box the generic value before comparing
with null; concrete nullable/union paths remain unchanged. The new runtime test
failed before correction and covers references, values, nullable values and
nullable reference bounds, for both is-null and is-not-null.

Validation: **55 semantic tests** and **14 focused/adjacent runtime tests pass** on
.NET 11. Targeted compiler builds, whitespace formatting and diff checks pass.
The generated red probe now emits successfully, but still fails at runtime with
a NullReferenceException in its generic ref child-cache helper; that separate
issue remains under investigation before committing the red emitter. The generator
suite at this point has **62 passing / 1 failing tests**, so no complete red-output
validation is claimed.

SemanticFacts and the CLI emitter are not yet ported; their future translations
must retain these fixes. The nominal relationship change is shared across targets;
neoCLR's native generic null-test representation remains target-owned. The CLI
bridge documentation records the boxed encoding and its replacement intent. No
native execution or completed bootstrap is claimed.


## PORT-052 — qualified source lookup and constructor-initializer ownership

The Raven red-node probe exposed a missing `: self(...)` call. Qualified source
types could appear absent during reentrant declaration lookup because canonical
metadata lookup intentionally omits source types at that point. Qualified
expression lookup now falls back to the indexed source declaration in the same
namespace, preserving arity and canonical metadata/target-provider selection.

Retaining diagnostics also exposed provisional initializer errors against
incomplete class hierarchies. Explicit constructor initializers now bind once per
semantic model after declaration binders are ready. Their diagnostics and cycle
errors belong to the model's declaration bag, not a replaceable class binder.
The implementation is isolated in SemanticModel.ConstructorInitializers.cs.
Constructor-initializer symbol queries complete declarations without eagerly
binding ordinary method bodies. No syntax or target-default changes are involved.

The reduced negative baseline had **182 passing / 2 failing tests**: invalid
qualified constructor calls lost their diagnostics in both compilation and document
queries. Focused tests now cover cross-file declaration order, unrelated global
names, persistent invalid-initializer diagnostics, emission rejection and existing
self/base delegation behavior. The generated red runtime probe now passes.

These components are not yet translated. Their Raven ports must retain the shared
lookup and initializer behavior; the neoCLR target consumes the same binder.
Validation executes on modern .NET, not a native runtime. The broad baseline
started before this slice stopped at RavenDocGenerationTests' source-only
extension-indexer link assertion; this is not a clean full-baseline claim.

Final focused validation: **303 adjacent compiler tests** and **63 C# generator
tests pass** on .NET 11, including the generated red-node runtime probe. Targeted
compiler builds, required generator/build script, whitespace formatting and diff
checks pass. The red emitter and its shared fixture are committed in the following
slice after validating the translated generator.


## PORT-053 — Raven red-node declaration output

Both generator implementations now provide RavenRedNodeGenerator.GenerateNode.
Output preserves abstract/concrete hierarchy declarations, green and typed
constructors with `: self(...)`, private child/list caches, positioned token
wrappers, Update/With identity and generic/non-generic visitor dispatch. Concrete
Raven classes retain their default sealing. Abstract nodes can emit closed-hierarchy
attributes. C# production output remains available and unchanged.

Shared C# tests parse repository output, compare both emitters exactly and execute
a generated red/green hierarchy against small authored runtime fixtures. The probe
covers explicit kinds, nullable children, ordinary node lists, token/child/list
positions, parent links, cached identity, detached updates and red visitor dispatch.
It passes in compilation-first and semantic-model-first diagnostic orders. The
constructor tests additionally exercise incremental document diagnostics directly.
Optional tokens, token/separated lists and closed-hierarchy metadata are covered by
source parsing/parity here, not complete runtime execution.

Validation: **64 C# generator tests / 72 Raven generator tests pass** on .NET 11.
The complete Raven tool library was rebuilt with an immutable C# compiler snapshot
from f2b613a41, then the shared suite was rerun. The required generator/build script,
targeted builds, whitespace formatting and diff checks pass. The tool rebuild
retains 42 existing source warnings and has no errors. No native execution or
completed compiler bootstrap is claimed.

Raven red factories, production syntax-runtime translation and CLI output-mode
selection remain subsequent slices. This fixture is deliberately not presented as
the production syntax runtime port.


## PORT-054 — cold source extension discovery

The broad baseline's RavenDoc source-indexer link failure was reduced to a compiler
API regression without running documentation generation: after adding an import
tree, LookupApplicableExtensionMembers returned no source extensions until full
diagnostics ran. The reduced baseline was **5 passing / 1 failing tests**.

Extension lookup now discovers candidate source files from a per-namespace syntax
index and ensures their member signatures before enumeration. Anonymous extensions
are indexed even without a declared type name; an explicit regression caught that
omission. Attributed source types/methods are conservatively included for CLI-style
extension declarations. Plain unrelated type files are excluded: the incremental
regression suite caught an initial over-broad implementation that declared 31 files
where the existing limit was two. The final index preserves demand-driven binding.

Tests cover cold/warm queries after AddSyntaxTrees, exact/prefix filters, methods,
properties, indexers, anonymous extensions and attributed source methods. The
original RavenDoc generation test has not been rerun; validation uses compiler API
and runtime tests, so a clean full baseline is not claimed. No documentation/site
generation is required for this follow-up.

This is shared compiler behavior, not target policy or new syntax. The C# binder,
compilation and source index are not yet translated; carry the fix into their Raven
ports. .NET remains the default; no native runtime validation is claimed.

Final validation: **456 compiler tests pass**, combining constructor delegation,
qualified/canonical lookup, incremental binder/compilation reuse, extension lookup,
source/metadata extension semantics and indexer runtime behavior. Both generator
suites were revalidated against this compiler: **64 C# / 72 Raven tests pass**.
Targeted builds, whitespace formatting and diff checks pass on .NET 11.

## PORT-055 — Raven red factory output

Both generator implementations now emit Raven red syntax factories alongside red
node declarations. Automatic overloads preserve explicit kinds, default tokens,
missing optional tokens and nullable children. XML definitions retain parameter
order, token/null defaults, aliases and escaped summaries. Abstract nodes return
no factory; malformed unsupported defaults retain the existing error contract.
The emitter is a partial class so factory logic remains separate from declarations
without duplicating type mapping or naming helpers.

Shared C# tests parse both factory modes for every repository node and compare
complete output between implementations. The generated fixture executes full and
convenience factories, aliases, default tokens, null children and lists under both
compilation-first and semantic-model-first query orders. Optional missing tokens
have source-level coverage. This is fixture runtime validation, not a completed
production syntax-runtime port. CLI selection and production runtime translation
remain separate work; C# generation remains intact.

The Raven translation explicitly unwraps nullable factory definitions within their
null guard. Default Raven nullability intentionally does not narrow after a null
check, so this follows the documented rule rather than introducing a language
change. No target policy changed; .NET remains the default.

Validation: **67 C# / 75 Raven generator tests pass on .NET 11**, including exact
repository output parity and generated factory execution. The required generator
build script, full Raven generator build, whitespace formatting and diff checks
pass. The Raven generator build retains warning diagnostics (46 warnings, zero
errors). No RavenDoc/site generation or native neoCLR execution was performed.
The broad baseline caveat recorded in PORT-054 remains unchanged.

## PORT-056 — complete Raven syntax output and driver selection

The runtime dependency audit found that the production source set could not yet
be assembled from the driver, and token factories still lacked Raven output.
Both generator codebases now have a pure source-set assembler which retains all
node/factory models, additional permitted hierarchy types and visitor/rewriter
variants. It produces 985 files for the repository models. C# filesystem/CLI
orchestration calls that assembler through `--raven-syntax-tree <directory>`;
validation and CLI porting remain separate work. C# stays the default output.

The explicit mode owns only `*.g.rvn` in `generated` and
`InternalSyntax/generated` beneath its destination. It cleans stale generated
Raven files and preserves authored files, C# output and the C# generation stamp.
A two-run CLI probe verifies deterministic contents and those ownership boundaries.

Runtime coverage found and corrected missing namespace qualification in green
token output. Literal review also found that Raven factory/facts text must escape
`$` to avoid interpolation; both emitters now preserve dollar signs, quotes,
backslashes and control characters. This fixes generator output, not a language
rule. Shared tests execute the generated token factory/facts code on .NET and
parse the entire source set. Cross-implementation tests compare all paths and
contents. The production runtime dependency closure remains the next porting work;
these tests do not claim that all generated nodes compile against a production
Raven syntax runtime yet. No target policy changed.

Validation on .NET 11: **69 C# / 78 Raven generator tests pass** (the two new C#
output tests were rerun after extending keyword-lookup coverage). Adjacent
syntax-facts/lexer coverage passes **143 C# / 146 Raven compiler tests** after
regenerating and rebuilding the Raven compiler port. The required build script,
full Raven tool build, formatting and diff checks pass. Raven builds retain
warning diagnostics: 50 in the generator, 403 in the existing compiler port,
zero errors. A final CLI probe verifies 985 stable files across two runs,
stale cleanup, missing-argument failure and preservation of authored/C# files
and the C# stamp. No RavenDoc/site generation or native neoCLR execution was
performed; the broad baseline caveat in PORT-054 is unchanged.

## PORT-057 — encoded string payload value semantics

The next dependency leaf of the syntax runtime is the encoded string payload used
by parsing, binding and hover formatting. `EncodedStringLiteralValue` is now a
sealed positional record class in C# and Raven, retaining text, encoding and the
interpolation marker. The encoding enum is ported alongside it. This follows the
porting directive for immutable data objects while retaining reference-type
storage. Constructor parameter names now match their properties, consistent with
the other positional record ports; current production call sites are positional.

The prior payload class used identity equality, so independently created equal
payloads also made containing lexer tokens unequal. Two focused regressions fail
before the change. Shared C# tests cover all three equality fields, ordinal text,
null/unrelated values, typed equality, operators, matching hashes, hash-set
membership, sealed reference-type shape and equality of tokens containing the
payload. `Token` itself is unchanged. Existing encoded-string diagnostics,
byte-emission and hover-formatting coverage validate the production consumers.

This is shared syntax data before target lowering, with no new language syntax or
runtime mapping. .NET remains default. The class is now part of the Raven compiler
port; the production expression parser and node runtime still require translation.

Validation on .NET 11: **104 shared token/lexer tests pass against each compiler
implementation**, plus **17 C# compiler tests** for encoded-string diagnostics,
byte emission and hover formatting. The initial baseline was 7 passing tests;
the two new equality regressions failed before the record conversion. Seed and
Raven project builds, formatting and diff checks pass. The first Raven project
build exited without diagnostics; its process was confirmed terminal, the record
compiled independently, and a retry using an immutable copy of the rebuilt seed
passed with 403 existing warnings and zero errors. No native neoCLR execution,
RavenDoc/site generation or clean full-baseline claim is made.

## PORT-058 — stabilize separated-list runtime contracts

The dependency review found two local defects before translating the list
runtime. `IsSoundSeparatedList` used node count as total slot count, rejecting
valid multi-node and trailing-separator lists. It now checks the complete
alternating sequence once without allocating a temporary array or enumerating
again. `GetSeparator` bounded indices by node count minus one, rejecting a stored
trailing separator; it now bounds against half the underlying slot count,
retaining constant-time checks and rejecting negative/out-of-range indices.

The list representation already supports trailing separators; this does not
change the language grammar's permission for them. Focused tests cover empty and
default lists, one/multiple nodes, trailing separators, invalid initial tokens,
adjacent nodes/tokens, wrong separator kinds at the end and wrong node types.
Accessor tests cover retrieval and invalid indices. The API docs explain the
node-count/slot-count distinction and trailing-separator behavior.

Validation on .NET 11: the adjacent baseline passed **9 tests**; the new soundness
cases reproduced **4 failures**, and the accessor cases reproduced **2 failures**.
After both fixes, **18 focused/adjacent tests pass**. The targeted core build,
whitespace formatting and diff checks pass. This is shared compiler syntax
behavior, not target policy; native neoCLR was not executed. Neither list helper
nor separated-list runtime is translated yet: their Raven ports must carry these
corrections and reuse the new C# tests. No RavenDoc/site generation was run.

## PORT-059 — source alias ownership during bootstrap binding

The production syntax-runtime translation exposed a bootstrap collision: a
qualified alias could retain a type from the referenced C# compiler assembly
instead of the matching Raven source declaration. Direct qualified base types
already selected source correctly. The alias path ran while declaration
completion was suppressed and accepted the metadata candidate too early.

Alias lookup now requests the exact indexed non-generic source candidate before
retaining metadata. Existing declaration guards handle re-entry; this does not
introduce eager whole-compilation declaration binding or a new public API.
Regression tests query the derived declaration before diagnostics, verify that
its base belongs to the source assembly, and exercise access to an internal base
member. Both file orders and direct/aliased base names are covered. The two alias
cases failed before the correction; direct qualification already passed.

Validation on .NET 11 rc1: **20 symbol-lookup tests and 155 adjacent namespace,
import, alias and incremental-compilation tests pass**. The pre-change lookup
baseline passed 16 tests. Core and compiler-driver builds, whitespace formatting
and diff checks pass. This is shared binding behavior before target lowering;
no native neoCLR execution was performed. The binder is not yet ported, so the
fix currently belongs to the C# bootstrap and must accompany its later Raven
translation. No RavenDoc/site generation was run.

The next runtime slice is still uncommitted: 27 authored Raven node, token,
trivia, list and visitor files parse without syntax errors, but semantic closure
and runtime parity are not established. Generated declarations, node methods,
rewriters and tree/diagnostic integration remain dependencies. Passing parser
checks must not be reported as a working syntax runtime, and bootstrap metadata
must not silently stand in for missing Raven declarations. The prior 54-test
node/list/annotation baseline passed before starting this translation.

## PORT-060 — trailing-trivia lookup and positions

Translating node navigation exposed two existing runtime defects. `FindTrivia`
stopped at the token's full span before checking trailing trivia, and a token's
trailing-trivia list started after token width without accounting for leading
trivia. Lookup now excludes only token text; the trailing list starts after both
leading width and token width. Both corrections are present in C# and the pending
Raven runtime translation. Public API shape and language syntax are unchanged.

Two focused regressions failed before the corrections. They cover tokens with
and without leading trivia, every position in trailing whitespace, token text,
leading trivia and the exclusive end boundary. **56 node/list/annotation tests
pass on .NET 11 rc1**, including both regressions. The earlier 54-test baseline,
targeted core build, whitespace formatting and diff checks pass. This is shared
syntax behavior; no native neoCLR execution or RavenDoc/site generation was run.

Runtime port work remains uncommitted and unvalidated semantically. The node
navigation methods, source-text writer and red rewriter are now translated too;
a parser-only check of all authored Raven compiler files reports zero errors.
The updated-seed full semantic pass exited 1 without printed diagnostics. A
second project-input pass was stopped after inspection showed that project
documentation options can schedule output even with `--no-emit`; subsequent
analysis uses explicit source inputs to avoid documentation generation. No successful
runtime build or shared-test parity is claimed for these pending files.

The pending `SyntaxReplacer` translation also parses. A reduced conditional-access
probe (`root?.Child.Width`, with nullable `root` and nonnullable `Child`) reports
RAV0402 on `Child.Width`: the parser ends conditional access before the following
member, and nullable conditional binding currently supports only a single access
or invocation. The existing **10 conditional-access runtime tests pass** but do
not cover this chain. Fix short-circuit chain ownership in parser/binder and add
observable null/non-null and evaluation-order coverage before translating this
case into a workaround. Parentheses must remain an explicit chain boundary;
carrier access and both target implementations also need review.
