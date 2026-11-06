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

Author direction, 2026-11-02: when the symbol interfaces are ported, remove their
`I` prefixes in both C# and Raven. Keep them as interfaces, update consumers and
generators together, and resolve collisions with concrete classes by naming their
implementation roles explicitly. This is an intentional API naming departure;
it is scheduled for the symbol component rather than the syntax-runtime slice.
See the [symbol naming policy](bootstrap-procedure.md#symbol-interface-naming-during-the-symbol-port).

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

## PORT-061 — conditional-chain stabilization (compiler slice validated)

The syntax-runtime port's `token?.LeadingTrivia.Width` exposed a missing
conditional-access chain boundary. Parsing now keeps subsequent member,
invocation and index trailers in the conditional branch; parentheses and other
postfix operators terminate it. Nullable and carrier binding share explicit
receiver-driven traversal. No ambient binder receiver state is introduced.

A new `BoundConditionalReceiverExpression` represents the already-evaluated
non-null receiver. The first emission implementation exposed why receiver
object identity is insufficient: lowering can copy executable receiver nodes.
The explicit marker survives generated rewrites and prevents re-evaluation.
The CLI emitter walks the branch's receiver chain from that supplied value.
Operations expose `IConditionalAccessInstanceOperation`, and member-reference
operations retain their instance as a child so cold semantic queries and
operation walkers can observe the complete chain.

Carrier tests exposed two additional existing defects: Optional extraction used
output-case metadata when the mapping changed payload type, and nested expression
generators lost the carrier payload local. Extraction now uses input metadata;
output construction remains separate. Payload locals use the existing lexical
emission scope instead of per-expression mutable overrides. Shared semantics
apply to both targets; only modern .NET execution is validated here.

The initial 10-test conditional-access baseline passed. Reduced chain tests failed
with RAV0402 before the parser/binder changes. New behavior tests cover null and
non-null receivers, chained calls/indexes, nested nullable access, nullable value
receivers, skipped arguments, single receiver evaluation, discarded values,
parenthesis boundaries, syntax round-tripping, cold type/operation queries and
Optional/Result success/failure with changed payload types. **44 focused tests and
337 tests in the final adjacent selection pass on .NET 11 rc1** (overlapping
selections, not additive totals). The adjacent selection covers conditional
access/attributes, nullable conditionals, carriers, propagation, pointers and
operations. A diagnostic-span regression found during consolidation was corrected.

The required generator/build script passed, and bound visitors/rewriters were
regenerated. Targeted builds, whitespace formatting and diff checks pass. The
repository baseline stopped after 68 successful batches with one stale
`SyntaxTriviaTest.TokenAttachedTrivia_HasCorrectPositions` assertion in the next
batch (`/tmp/node-runtime-chain-baseline.log`). It expected offset 6 after
`"  return"`, rather than offset 8 including leading trivia; this follows from
the previously committed trivia-position correction. The expectation is being
updated with empty/space/tab leading-trivia cases. No clean full-baseline result
is claimed yet. RavenDoc/site generation is disabled;
ordinary XML comment sidecars remain enabled for bootstrap metadata. Language,
grammar, website-reference text, operations API, CLI bridge and changelog notes
are updated; token highlighting does not need a new token rule.

The 31-file authored runtime translation remains uncommitted. The direct-source
semantic analysis preceding this fix completed with 89 errors, distinguishing
remaining dependencies/translation issues from parser success. It must be rerun
using the stabilized seed, then complete the missing runtime dependencies and
run shared C# tests against the Raven implementation. The binder/parser/emitter
are not ported yet; their eventual translations must retain these corrections.

Follow-up while the baseline runs: the internal syntax rewriter and public
syntax walker are translated (33 authored runtime files pending), and all
currently authored Raven compiler files pass parser-only checks. The stabilized
seed's direct-source semantic analysis completed with **83 errors**, down from
89; this is still not a working runtime build. The pass preceded these last two
translations.

Two independent reduced repros identify further stabilization work:
`func Get() -> Node => null!` reports RAV1503 despite the reference-suppression
contract, and `a![0]` reports RAV0030 because the bang/index combination is
recognized as a macro form. Bracket macro arguments are currently documented as
reserved, unimplemented design direction. Reconcile that reservation with
ordinary suppression/indexing before changing port source syntax. Simple alias
and aliased override probes with bootstrap metadata (including multiple files)
succeed; the remaining runtime override diagnostics need a more representative
reduction rather than assuming another alias-resolution bug.

Bang disambiguation now has an explicit syntax rule documented in
`bang-resolution.md`: `foo!(...)` and `foo![...]` retain their macro-shaped
interpretation regardless of a nullable value with that name. The port uses
`(a!)[i]`, `(b!)[i]` and `(Green!)[index]` where explicit unwrapping followed by
indexing is intended. Conditional invocation/indexing remain `foo?(...)` and
`foo?[...]`. A direct compiler probe accepts all four conditional/parenthesized
value forms (with the expected two suppression warnings). Thus the indexing
case is an intentional syntax distinction, not a compiler nullability defect.
The separate target-typed `null!` repro remains unresolved.

The refreshed direct-source analysis includes **70 Raven files** and completes
with **81 errors** (`/tmp/node-runtime-analysis-current.log`). This is a larger
source set than the preceding 83-error run, so the counts are not a like-for-like
measure of stabilization. Remaining errors include missing generated/source
ownership, nullability annotations, override matching, method hiding, target-typed
`null!`, and ordinary out-value copying. No runtime parity is claimed.

A separate compiler-API probe isolates the nullable-array alias override defect:
`Green?[]` and `G?[]`, where `alias G = Shared.Green`, resolve to array symbols
whose nullable element wrappers contain respectively the source type and its
alias. The alias targets that same source type. `SymbolEqualityComparer.Default`
returns false, while `IgnoringNullability` returns true. The current comparer has
structural array handling but no structural nullable-wrapper handling when
nullability is included; wrapper names retain the alias spelling. The next fix
must preserve nullable distinctions and equality/hash consistency, with regression
coverage for alias-based overrides, rather than discard nullability to make the
port compile. Probe: `/tmp/raven-alias-inspect.log`. This diagnosis does not yet
establish that all remaining override errors share this cause.

Nullable alias regression preparation: the existing focused equality/override
baseline passes **21 tests** (`/tmp/nullable-alias-baseline.log`). The new
`NullableAliasSignatureTests` covers nullable reference/value/constructed-generic
aliases, array and jagged-array nesting, equality/hash collection lookup, and
cold-versus-diagnostics-first override queries. All **11 cases fail** against the
current compiler. These exact C# test methods were compiled and invoked in an
isolated temporary harness referencing the existing compiler/test binaries
(`/tmp/nullable-alias-regression-before.log`); the repository test assembly and
ongoing full-baseline binaries were not rebuilt. The tests remain uncommitted
until the fix and normal test-runner validation are complete.

The stale attached-trivia expectation is corrected in `3ffdc66ee`, with **42
adjacent syntax tests passing**. Nullable alias identity is corrected in
`501fb3e58`: **41 focused and 185 adjacent tests pass** (overlapping selections),
including the 11 cases first reproduced in the normal test runner. Nullable
wrappers now compare/hash their underlying semantic type; existing equality
across differing nullable ABI projections remains intact. The comparer is not
yet translated to Raven; its future port must preserve this shared rule.

The macro-adjacency clarification additionally applies to every macro name,
not just keyword aliases. Tests cover ordinary/qualified names, spaces/tabs/
comments, and declaration positions; `if !foo()` remains ordinary syntax while
`if! foo()` is an expression-header macro candidate. During negative declaration
coverage, malformed type-member recovery was observed to drop the final closing
brace in `ToFullString`; this is a separate recovery gap, not a reason to accept
spaced macro names. Adjacency tests assert the macro classification, while valid
expression cases also require diagnostics-free parsing and text round-tripping.

Macro adjacency (`7dfac1245`) passes **68 focused parser tests and 601 adjacent
macro/conditional-access/suppression tests** (overlapping selections). The
refreshed 70-file port analysis completes with **82 errors** in
`/tmp/node-runtime-analysis-nullable-alias.log`: relative to the prior 81-error
run, one ambiguous call moves from one `With` use to the next and one additional
`SyntaxToken.With` override diagnostic appears. The broader source/metadata
ownership and override failures are therefore not resolved by the nullable-alias
identity fix alone.

The next isolated gap is ordinary value copying from scoped references.
`GreenSpanHelper` assigns `span` and then `fullSpan = span`, but the compiler
mistook that copy from an implicitly scoped `out` parameter for a scoped-value
escape. The 38-test scoped baseline passed; eight added diagnostics regressions
failed with RAV0353 before the change. Restricting write-through assignment
escape checks to ref-like values now passes **51 focused tests**, including a
.NET runtime test proving that changing the source after the copy does not alter
the destination. Existing scoped ref-like escape rejection remains covered.
**267 adjacent ref-safety tests pass** (overlapping the focused selection), and
the original reduced `out int` CLI repro now analyzes successfully. The shared flow analyzer has not
yet been translated to Raven; the direct Raven helper needs no workaround.

Scoped value-copy correction is committed as `6ab6c9e1d`. Baseline continuation
uses `/tmp/raven-baseline-remainder.sh`, a temporary copy of the repository script
that starts at the failed batch (`SyntaxOwnershipContractTests` through
`TargetCoreSelectionTests`), uses the already built compiler-test assembly, and
then runs the unchanged remaining-project loop. Log:
`/tmp/node-runtime-chain-baseline-remainder.log`. This continuation includes the
subsequent validated fixes; it is not represented as one unchanged-snapshot full
baseline run. The first 68 compiler batches passed before the trivia assertion
stopped the original run.

The baseline continuation completed all **11 remaining compiler batches** and
passed **300 C# compiler-parity tests**. It is now building the unfinished Raven
syntax-runtime project for Raven parity; a clean port build is not established.

Target-typed `null!` stabilization has a **26-test passing baseline** and five
new C# diagnostics/semantic-query regressions that fail before the fix. Reduced
CLI probes cover expression/body returns, typed local initialization, assignment
and arguments. A separate `(null)!` probe reports RAV0103 (`null` is not in scope),
which needs independent parser investigation. The reference-target suppression
implementation and runtime-null regression are being prepared; ordinary null
conversions and nullable-value extraction must remain unchanged.

The baseline continuation is terminal: all remaining compiler batches and the
C# parity project passed, then the unfinished Raven project failed with **81
errors**. The ordinary `out`-copy diagnostic is absent after its fix. Later test
projects were not reached. This is not a clean repository-wide baseline or a
successful Raven runtime port; it is validation of the compiler slice plus an
explicit failure of the still-uncommitted dependency translation.

The target-typed `null!` implementation passes **32 focused tests**, including
runtime-null behavior and cold type/operation agreement after fixing two
available-state query regressions. The two negative conversion cases and **439 adjacent tests pass** on .NET 11
rc1 (overlapping the focused selection). The original `GreenNode` reduced
CLI repro now succeeds with RAV0403. A separate overload probe (`Pick(string)` /
`Pick(object)` with `Pick(null!)`) still reports RAV1501 when candidates supply no
common reference target. This is a remaining compiler gap to fix, not a language
restriction or a porting workaround. The `(null)!` parser gap remains separate.


## PORT-062 — green runtime null-input contracts

The direct translation exposed nullable operations on inputs that were annotated
non-null despite accepting null as empty. Green `SyntaxNode` slot arrays and
`SyntaxToken` trivia-enumerable inputs now carry honest nullable annotations in
both sources. The C# token convenience constructor's default-null trivia inputs
are nullable too, matching the Raven translation. Green `SyntaxList` still
requires an item array and throws `ArgumentNullException("items")`; its base
initializer uses an explicit null test in both languages to preserve that behavior
without nullable access on a statically non-null input.

The initial syntax-runtime baseline passed **50 tests**. New tests confirmed
three existing runtime behaviors and failed the nullable metadata check before
the correction. **63 adjacent tests pass** after the change. A follow-up test
strengthens null-trivia clearing coverage using a token with nonempty leading and
trailing trivia, checking that the opposite side and original token are preserved.
Raven parser validation reports zero errors; full Raven runtime compilation and
parity are still pending, so its mirrored runtime files remain uncommitted.

A full-source compiler-API probe now identifies the remaining override failure:
`GreenNode.With` uses source-owned types, but derived `With` signatures retain
aliases to the C# bootstrap `GreenNode` and `SyntaxAnnotation`. Nullable-array
identity is no longer the issue. Four reduced CLI probes reproduce the collision
for both namespace styles and both matching/different alias names. Permanent
regressions additionally vary syntax-tree order and assert signature ownership,
not just the absence of diagnostics. Logs: `/tmp/raven-port-signature-inspect.log`
and `/tmp/raven-alias-ownership-matrix.log`.

The strengthened null-input tests and existing symbol-lookup baseline pass
**24 tests**. All **eight new source-ownership regressions fail** before the
next alias fix, consistently finding bootstrap-owned types in derived signatures
(`/tmp/source-port-alias-ownership-before.log`).


## PORT-063 — source alias ownership during ordinary declaration setup

The exact indexed-source lookup added in PORT-059 ran only when namespace
completion was explicitly suppressed. During ordinary source declaration setup,
metadata lookup can also return a provisional bootstrap type. Removing that
narrow guard lets the existing exact-name declaration path select the source
symbol before an alias retains metadata. Existing re-entry guards remain in
charge; no public cache API or eager whole-compilation declaration pass is added.

All eight new ownership regressions failed before the change. **39 focused
symbol-lookup and nullable-alias tests pass** afterward, covering both source-file
orders, block/file-scoped namespaces, matching/different alias names, source-owned
return and nullable-array parameter types, and actual override matching.
**648 adjacent namespace/import/alias/incremental tests pass**, excluding
documentation-generation tests. The new 70-file semantic analysis uses
`/tmp/raven-source-alias-seed` and writes
`/tmp/node-runtime-analysis-source-alias.log` (results below). Validation ran on
modern .NET; native neoCLR execution was not tested.
This shared binder correction must be preserved when the binder itself is ported.

The initial broad adjacent filter included `NamespaceDocumentationTests`, which
can invoke documentation-site generation in temporary directories. That run was
explicitly interrupted after this was detected; no pass is claimed for it. The
replacement filter excludes `FullyQualifiedName~Documentation` and writes
`/tmp/source-port-alias-ownership-adjacent-no-docs.log`. Keep that exclusion in
future broad porting filters rather than assuming a namespace filter is docs-free.

The refreshed analysis is terminal with **47 errors** across the same 70 Raven
files. It contains **no RAV0307 override errors**, no RAV1504 assignment-type
errors, and no scoped out-copy diagnostic. Source alias ownership is therefore
materially improved in the real translation, not only the reduced fixture.
Remaining failures include missing generated/runtime dependencies, nullable
casts/annotations, method hiding, generic node conversion, and two newly exposed
RAV0924 diagnostics for nullable `params IEnumerable<SyntaxTrivia>?` inputs.
Nullable params-collection recognition needs stabilization; do not undo the
correct null-input annotations to conceal that gap. The port still does not
compile or execute as a complete syntax runtime.


## PORT-064 — nullable collector parameters

The translated green token methods intentionally accept nullable trivia
collections. Shared parameter validation inspected the nullable wrapper instead
of the collection, rejecting these methods with RAV0924; nullable local collectors
also lost expanded-call behavior. Validation now examines the underlying type
while retaining the declared nullable parameter type. Nullable value-type
collections remain distinct: `ImmutableArray<int>? ...` still collects nullable
values into `IList<ImmutableArray<int>?>`. Existing argument binding
already handles the underlying element type. CLI parameter-array metadata now
recognizes nullable arrays too.

The baseline passed **15 tests**. Three new semantic regressions failed before
the fix; **22 focused tests pass** afterward, including runtime checks for nullable
arrays, IEnumerable and IList collectors. Omitted arguments produce empty
collections, expanded arguments preserve element count, and normal-form null
arguments remain null. The runtime test uses explicit suppression after its null
guard, following Raven's existing explicit-unwrapping rules.

This is shared binder behavior with a CLI metadata correction, not a neoCLR-only
policy. Validation ran on modern .NET; native neoCLR execution was not tested.
The binder and emitter are not yet translated, so the correction must accompany
their future port. Existing Raven nullable-trivia signatures remain intact.
**483 adjacent invocation/overload tests pass**, using the overload-resolution
suite filter (including its runtime overlap) and excluding documentation tests.
The refreshed analysis removed both RAV0924 errors but exposed two RAV0307
failures in directive/skipped-token trivia, leaving **47 errors**. A same-order
comparison against the preceding compiler confirmed that source order alone did
not explain the difference. Further investigation is recorded in PORT-065.

Validation artifacts: `/tmp/nullable-params-baseline.log` (15 pass),
`/tmp/nullable-params-before.log` (three expected failures),
`/tmp/nullable-params-final.log` (22 pass), `/tmp/nullable-params-adjacent.log`
(483 pass before the final reference-only guard), and the two same-order full
analyses `/tmp/node-runtime-analysis-source-alias-sorted.log` and
`/tmp/node-runtime-analysis-nullable-params.log`. Formatting and targeted compiler
build pass. No documentation-site generation was run for this slice.

## PORT-065 — source ownership in preliminary imported signatures

The apparent declaration-completion inconsistency came from the preliminary
signature pass preferring metadata over an already-declared source type. A
bootstrap `DiagnosticInfo` in the base signature temporarily disagreed with the
source-owned parameter in its override. That caused the intermediate method to
lose its virtual flag, and derived overrides then reported RAV0307. Later symbol
queries completed the source signatures, obscuring the initial mismatch.

The fix changes ownership precedence in `ResolveExactImportedSkeletonType`:
registered source types precede metadata. Wildcard imports, exact imports and
qualified names all use this path. No eager declaration traversal, diagnostic
suppression, or Raven-source workaround is added. Broader experimental import
lookup changes did not fix the problem and were reverted.

The override baseline passed **18 tests**. Three focused preliminary-signature
regressions fail before this correction, including nullable array element
ownership. **65 focused tests pass** afterward. This is shared signature-binding
behavior; the pass is not yet ported and must retain this rule in its Raven
translation. Validation is on modern .NET, not native neoCLR. The refreshed
70-file analysis has **45 errors**, with no RAV0307 or RAV0924 diagnostics; the
remaining failures concern other runtime translation/dependency gaps. **1,187
adjacent import, namespace, alias, incremental, invocation, and override tests
pass**, excluding documentation-generation tests. The nullable-collector and
signature-ownership corrections are committed together as this stabilization
slice; the unfinished Raven syntax-runtime files remain uncommitted. Logs: `/tmp/skeleton-import-before.log`,
`/tmp/skeleton-import-focused-after.log`, `/tmp/skeleton-import-adjacent.log`, and
`/tmp/node-runtime-analysis-skeleton-import.log`.


## PORT-066 — default-token and trivia-list nullability

The red token's green reference can be null in a default struct value. Its field
is now explicitly nullable in C# and Raven; guarded Raven accesses use explicit
unwraps, preserving the existing default-token branches. Green trivia lists
accept a null array as empty, so their constructor input is nullable. Their
checked slot override returns a non-null node or throws for an invalid index.
Width is calculated once from the normalized array and reused for full width.
These are local contract/cleanup changes, not new language semantics.

The focused baseline passed **26 tests**. Added runtime checks passed before the
change while the nullable metadata assertion failed. **46 focused and adjacent
token/trivia/annotation/ownership tests pass** afterward. The compiler build and
Raven parser validation pass (zero syntax errors). Full Raven semantic analysis
now reports **31 errors**, down from 45. Conditional access on incorrectly
non-nullable token/trivia inputs is resolved. Remaining errors include a `Sum`
overload-selection issue (decimal result for an int-valued selector), nullable
token consumers in replacement code, and other existing runtime/dependency gaps.
The mirrored runtime files remain uncommitted until the runtime dependency slice
compiles. Validation is modern .NET only. Logs:
`/tmp/runtime-null-contract-before.log`, `/tmp/runtime-null-contract-after.log`,
and `/tmp/node-runtime-analysis-null-contract.log`.


## PORT-067 — numeric selector overloads and static extension calls

The direct `SyntaxTriviaList.CalculateFullWidth` translation exposed selection of
LINQ's decimal Sum overload for an int-valued selector. A reduced probe reproduced
it independently of the runtime sources. Overload ranking now compares callable
result conversions for synchronous lambdas whose candidate input signatures
agree, rather than treating an earlier candidate's provisional delegate as the
argument's natural type. The same helper covers nominal and structural callables.

The static form exposed an adjacent gap: type qualifiers were treated as implicit
extension receivers, and callback pre-inference stopped with multiple generic
candidates. Type qualifiers now contribute no implicit argument; ordinary
arguments infer each candidate's own type parameters, with natural bindings reused
within the pass. No Raven-port source workaround was introduced. The binder and
overload resolver remain C# and must preserve this behavior in their future port.

The initial overload baseline passed **71 tests**. Four of six new Sum cases
failed before the fix. **26 focused tests pass** now, covering int/long/float/
double/decimal results, static and extension calls, execution on .NET, declaration
order, value versus unit results, and ambiguity between unrelated numeric callback results. NeoCLR checks
exercise semantic binding of structural callables only, matching the existing
fixture; they do not validate its CLI profile or execute a native runtime.
Adjacent overload/pipe tests and a fresh 70-file analysis are running. Logs:
`/tmp/sum-selector-before.log`, `/tmp/sum-selector-final-focused.log`,
`/tmp/sum-selector-adjacent.log`, and `/tmp/node-runtime-analysis-sum-selector.log`.

The first adjacent run found three failures (527 passed). One exposed a lost
value-over-unit callback preference, now corrected and covered for both semantic
targets. Two stale tests expected `Predicate<int>` versus `Func<int, bool>` to
resolve without ambiguity; a reduced C# build reports CS0121 for that same call.
Those tests now require RAV0121 while retaining delegate-target and replay checks;
tautological non-negative counters were removed. See
`/tmp/callback-ambiguity-csharp.log`. The refined pre-inference path only expands
to multiple candidates when a lambda is present. **26 focused tests pass** in
`/tmp/sum-selector-verified-focused.log`; final adjacent and full-port validation
are running in `/tmp/sum-selector-adjacent-final.log` and
`/tmp/node-runtime-analysis-sum-selector-final.log`.

Final validation passed **534 adjacent overload, callback, and pipe tests**. The
final 70-file analysis reports **30 errors**, with the Sum selector conversion
error resolved. Remaining failures belong to nullable runtime consumers and
other existing translation/dependency gaps. Targeted builds and whitespace
formatting completed. No documentation-site generation was run.


## PORT-068 — nullable green-list slots

Green list `With` already accepts `GreenNode?[]`, and traversal/width calculation
already skip empty slots. Its constructor, storage and indexer incorrectly
claimed non-null children. Those annotations now agree in C# and Raven while
the array itself remains required. This removes the port's incompatible array
constructor call without altering nullable-slot behavior or adding conversions.

The baseline passed **29 tests**. New empty-slot traversal/width/update checks
passed before the change; the metadata assertion failed. **48 focused and
adjacent tests pass** after the correction; targeted build, whitespace formatting
and Raven parser validation pass. The fresh full-source analysis is still running
(`/tmp/node-runtime-analysis-green-list-slots.log`). The Raven runtime files
remain uncommitted while their dependency slice is incomplete. Modern .NET was
tested; native neoCLR was not executed. A separate reduced `out var` conditional probe succeeds, so the
remaining scope diagnostics require more specific reductions. Logs:
`/tmp/green-list-slots-before.log`, `/tmp/green-list-slots-after.log`, and
`/tmp/raven-out-scope.log`.


## PORT-069 — required structured-trivia child slots

Directive trivia and skipped-token trivia require child zero, while the general
green-slot API is nullable. Their red accessors and green `With` implementations
now explicitly assert the required slot before the existing cast in C# and Raven.
Reference suppression adds no runtime null check and preserves the existing
required-child invariant. This is a direct translation clarification, with no
new language behavior. The initial structured-trivia/ownership baseline passed
**44 tests**; post-change checks are running.

The PORT-068 analysis completed with **38 errors**. The incompatible green-list
constructor call is resolved; nullable indexed slots expose nine diagnostics in
span helpers, rewriters and red list accessors that previously assumed non-null
items. Those consumers require explicit invariant handling. PORT-069 passes
**53 focused/adjacent tests**, the targeted build, formatting, and Raven parser
validation. Modern .NET was tested. Its refreshed analysis is running in
`/tmp/node-runtime-analysis-required-trivia-slots.log`. The unfinished Raven
runtime sources remain uncommitted.


## PORT-070 — spans across empty green slots

The nullable-slot audit exposed a shared runtime defect: `GreenSpanHelper`
dereferenced empty boundary slots. Both sources now scan to the first/last
non-empty slot before trimming trivia; all-empty lists yield empty spans at the
supplied position. The baseline passed **35 tests**, and the new regression
failed with NullReferenceException before the fix. **44 focused and adjacent
tests pass** afterward, together with the targeted build, whitespace formatting,
and Raven parser validation (zero errors). Modern .NET was tested. The mirrored
Raven runtime remains uncommitted pending its dependency closure. The preceding
PORT-069 full analysis is still running and does not include this span change.
Logs: `/tmp/green-slot-spans-before.log`, `/tmp/green-slot-spans-after.log`.


## PORT-071 — typed-list elements and green rewrite buffers

Typed node/token/separated-list accessors now explicitly assert populated element
slots after indexing a general green list. The green rewriter's copy buffer is
`List<GreenNode?>`, matching its existing prefix-copy behavior for nullable slots.
Both changes are mirrored in Raven and preserve runtime behavior. Existing list,
slot, ownership and separated-list baselines passed **60 tests** across two runs.
Post-change checks pass **60 tests**, targeted build, whitespace formatting and
Raven parsing (zero errors). Validation is modern .NET only. A fresh full-source
analysis is running in `/tmp/node-runtime-analysis-typed-list-slots.log`,
including PORT-070 and this slice. The mirrored Raven runtime remains
uncommitted until its dependency closure compiles. The previous PORT-069 analysis completed with
**34 errors**; it did not include PORT-070's span correction.


## PORT-072 — inherited same-signature method ambiguity

The PORT-071 analysis completed with **25 errors**, down from 34, including the
still-ambiguous green rewriter calls. A reduced derived/base method pair reproduces
RAV0121 independently. Shared overload ranking now prefers the derived class
declaration when parameter signatures and generic arity agree and ordinary
argument conversions are tied. Different base overloads remain available.

The baseline passed **79 tests**. Four new semantic regressions failed before
the correction, varying source order and explicit `new`. A runtime regression
checks derived dispatch alongside a different inherited overload. Post-change
validation passes **84 focused tests**, including runtime execution on modern
.NET. Broader tests and fresh 70-file analysis are running in
`/tmp/derived-method-adjacent.log` and `/tmp/node-runtime-analysis-derived-method.log`.
Native neoCLR was not executed. The shared resolver is not yet ported; preserve this rule
in its future Raven translation. Logs: `/tmp/derived-method-before.log`,
`/tmp/derived-method-after.log`.

Final validation passed **527 adjacent overload/virtual/callback tests**. The
70-file analysis now reports **23 errors**, with both green-rewriter RAV0121
diagnostics resolved. Builds, formatting and diff checks pass. A follow-up
reduction succeeds for `FirstOrDefault()` on a source record struct but fails
for `FirstOrDefault(predicate)`; investigate overload inference rather than
working around that difference in Raven runtime sources.


## PORT-073 — annotation-only struct invocation results

`FirstOrDefault(predicate)` retained an annotation-only wrapper on a source
record-struct return value, incorrectly requiring nullable unwrapping. The shared
`NullableMetadataFacts.GetReturnType` invocation projection now returns the
underlying known value type for AnnotatedUnderlyingType, preserving declared
method annotations and genuine NullableValueType wrappers. This avoids changing
symbol identity, conversion caching or runtime ABI. Preserve this helper rule
when porting it to Raven.

The initial baseline passed **17 tests**. The predicate regression failed before
the correction while the parameterless form passed. Post-change checks include
genuine nullable struct results and execution on empty/populated sequences.
Logs: `/tmp/first-default-nullability-before.log` and
`/tmp/first-default-nullability-after.log`. **22 focused tests pass**, including
.NET runtime execution. Broader nullable/overload tests and refreshed full-source
analysis are running in `/tmp/first-default-nullability-adjacent.log` and
`/tmp/node-runtime-analysis-first-default.log`. Native neoCLR was not executed.

Final validation passed **753 adjacent nullable, overload, virtual-member and
runtime tests**. The 70-file analysis reports **22 errors**, with the predicate
FirstOrDefault conversion resolved. Builds, formatting and diff checks pass.
A separate reduced case still rejects `(Node)value` for `T where T: Node`,
with Node explicitly open; see `/tmp/raven-generic-base-cast-open.log`. Investigate
constraint conversions next rather than weakening the translated generic APIs.


## PORT-074 — class-constrained type parameter conversions

The shared reference-conversion classifier now follows declared class constraints
and chains through type parameters. It previously walked only BaseType, which
source type parameters do not expose, rejecting the direct generic translation
of node replacement code. Constraint traversal tracks visited parameters.
The conversion remains a reference conversion and does not admit unrelated
classes. Preserve this classifier behavior when porting it to Raven.

The baseline passed **83 tests**. Four regressions failed before the fix, covering
implicit/explicit syntax and direct/chained constraints. These and a .NET runtime
reference-identity test pass (**5 focused tests**). A negative unrelated-class
case also passes: **89 focused and adjacent tests pass** in
`/tmp/constraint-conversion-focused-final.log`. Broader conversion/generic/constraint/
overload validation is running in `/tmp/constraint-conversion-adjacent.log`, and a
fresh 70-file analysis in `/tmp/node-runtime-analysis-constraint-conversion.log`. Logs: `/tmp/constraint-conversion-before.log`
and `/tmp/constraint-conversion-after.log`. Native neoCLR was not executed.

The refreshed 70-file analysis completed with **19 errors**, down from 22,
resolving the constrained generic node conversions. Broader validation remains
running. A separate reduced LINQ tuple-label probe fails because unnamed tuple
expressions do not infer identifier labels; see `/tmp/raven-tuple-label-query.log`.
Investigate that language gap next instead of rewriting away tuple usage.

The broad run completed with **1,269 passing tests and one pre-existing failure**:
`GenericExtension_GetType_WithLambdaPredicate` used a redundant conditional access
after the non-null GetParameters array. The preceding compiler fails identically
(`/tmp/constraint-conversion-existing-failure.log`). Its sample now uses
`GetMethod?.GetParameters().Length`, preserving the first nullable receiver guard,
and removes a redundant public modifier. It is classified Target=DotNet. The
repaired test, adjacent member-binding class and new constraint cases pass
**14 tests** (`/tmp/constraint-conversion-cleanup-after.log`). No unchanged broad
run is claimed fully green; the only failure was separately reproduced and
repaired. The compiler correction and local fixture cleanup are committed as
this slice. No documentation-site generation was run.


## PORT-075 — inferred tuple element names

The direct `(node, index)` translation lost names required by subsequent LINQ
selectors. The shared tuple binder now infers unique identifier/member-access
names, suppressing duplicate and reserved inferred names while retaining explicit
labels and target tuple names. C# reference behavior was checked against the
[tuple reference](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/value-tuples)
and Roslyn's reserved-name classification. Tuple runtime mappings are unchanged.
Existing field/symbol lookup and highlighting need no new syntax or API; grammar
syntax is unchanged. The website reference index and type rules are updated
without running documentation generation.

The baseline passed **16 tests**. Three new name-inference cases failed and three
fallback cases already passed before the change. The reduced LINQ query now
compiles. Focused semantic/runtime checks, broader validation and full-source
analysis are pending. The binder is not yet ported; retain this shared behavior
when translating it. Logs: `/tmp/tuple-inferred-names-before.log`,
`/tmp/tuple-inferred-names-focused-final.log`.

The focused run passes **24 tests**, including .NET LINQ execution and neoCLR
structural tuple semantic binding (no native execution). Broader validation and
full analysis are running in `/tmp/tuple-inferred-names-adjacent.log` and
`/tmp/node-runtime-analysis-inferred-tuple.log`. The six name tests now query
GetTypeInfo before diagnostics to exercise cold binding; rerun that strengthened
filter after the current broader run completes. This slice remains uncommitted.

The broader run completed with **253 passing tests and one scripting failure**:
`RunAsync_UsesDefaultPreludeForLinqCollectionExpression` loads both the local
Raven.Macros assembly and NuGet version 0.1.16, reporting duplicate macro exports.
This needs a preceding-compiler comparison before attribution. Cold tuple query
checks are running in `/tmp/tuple-inferred-names-cold.log`; the full-source
analysis remains active.

All **six strengthened cold-query tests pass**. The full 70-file analysis completes
with **17 errors**, resolving both tuple-label failures. The scripting case
passes in isolation on both the preceding and current compiler; its broad-run
failure is process/reference-state dependent. `RuntimeMetadataReferenceResolver`
imports loaded host assemblies and ScriptOptions.Default captures them, while
the prelude can also resolve package macro assemblies. Do not claim the broad
run fully green: **253 cases passed**, with the one duplicate-export failure
tracked for separate scripting/reference-isolation work. Tuple feature checks,
formatting and builds pass, and no documentation generation was run.


## PORT-076 — class-constrained reference-type classification

The remaining GetRed null return is not a nullable member-API issue: source and
PE type parameters classified reference types only from the explicit class flag,
ignoring named class constraints. Both now use shared TypeParameterFacts traversal
for class and chained constraints, excluding object/ValueType/Enum roots and
interface-only bounds. It is cycle-protected and avoids provisional-state caching.
This foundational helper must be translated with both symbol implementations.

The baseline passed **25 tests**. Four new source/metadata regressions failed
before the fix; interface/value/enum negative cases already passed. Post-change
validation includes execution of a suppressed-null return with a class-constrained
generic return type. Logs: `/tmp/class-constraint-reference-before.log` and
`/tmp/class-constraint-reference-after.log`. Checks are running.

**33 focused checks pass**, including .NET runtime execution. Broader tests and
full-source analysis are running in `/tmp/class-constraint-reference-adjacent.log`
and `/tmp/node-runtime-analysis-class-reference.log`. Source tests are strengthened
to request declared method symbols before diagnostics; rerun that cold-query
filter after the broader test process completes. This slice remains uncommitted.

Final validation passes **1,388 broader generic, constraint, nullable and conversion
tests**, plus **eight strengthened cold/source/metadata/runtime checks**. The
70-file analysis now reports **16 errors**, resolving GetRed's constrained null
return. Builds, formatting and diff checks pass. Modern .NET was executed;
native neoCLR was not. No documentation-site generation was run.


## PORT-077 — .NET nullable wrapper member lookup

The direct GetValueOrDefault translation requires member lookup on the .NET
nullable wrapper, rather than on its contained value. A new target contract
provides a member lookup type only for genuine NullableValueType wrappers and
only when the requested instance member belongs to that runtime wrapper. The
base contract returns no mapping; DotNetRuntimeContract owns System.Nullable<T>
construction. The binder retains normal nullable diagnostics for other members.

Initial testing found a separate special-type mapping defect: System_Nullable_T
resolved the non-generic System.Nullable helper because its metadata name omitted
arity. The CLI contract now uses System.Nullable`1. A definition-arity regression
locks this distinction. The baseline passed **69 tests**, and all three initial
runtime-member cases failed before the changes. Follow-up runtime/special-type
checks are running in `/tmp/nullable-runtime-members-after2.log`. This slice is
uncommitted pending validation.

The corrected generic definition binds members, but initial method runtime tests
returned wrong values: invocation emission did not recognize the nullable wrapper
and constructed Nullable<T> as the same value receiver shape and boxed an already
loaded address. The CLI emitter now recognizes that exact representation pair.
**Four runtime/special-type tests pass** after the fix, including absent/present
values, fallback arguments, HasValue and Value's absent-value exception. Target
policy checks were added to keep nullable reference behavior and neoCLR ownership
explicit. Broader validation and cold semantic queries remain outstanding.

Final validation: **14 focused tests** pass, including cold member-symbol queries,
contained-member diagnostics and runtime execution of a nullable generic source
struct. **589 broader nullable/member/conditional-access/target-contract tests**
pass. The fresh **70-file port analysis falls from 16 to 9 errors**; fixing list
wrapper lookup also removes downstream out-variable failures. The remaining
errors involve generated-node dependency closure, source/bootstrap ownership,
conditional token typing and explicit null handling in unfinished translations.
Builds, whitespace formatting and diff checks pass. Modern .NET was executed;
neoCLR validation is contract-only. No RavenDoc/site generation was run.
The slice is ready to commit; unfinished syntax-runtime Raven files remain
unstaged until their dependency closure compiles.

## PORT-078 — arbitrary-length array attribute constants

The next dependency audit assembles all **985 generated Raven syntax files** with
**66 authored runtime files** outside generated directories (1,051 inputs). This
expands validation beyond the earlier 70-file partial runtime. The first analysis
crashes in `AttributeDataFactory` while binding a closed-hierarchy attribute:
`MoveToImmutable` requires count equal to capacity, but the builder has default
capacity and accepts arbitrary input lengths.

A **43-test attribute/hierarchy baseline** passes. New reduced source-query and
metadata tests reproduce **six failures**, with one existing capacity-aligned
case passing. Shared binding now uses `ToImmutable`, retaining the single-pass
collection traversal and ordered typed constants. `AttributeDataFactory` has not
yet been translated; future Raven translation must retain the corrected behavior.
This is a shared correction for both target pipelines, with no new source syntax,
editor grammar change or metadata encoding. Native execution is not claimed.
The corrected complete-source analysis and regression checks are running.

Validation passes **50 focused attribute/hierarchy tests** and **366 broader
attribute/closed-hierarchy tests** on modern .NET, including emitted type-array
metadata. The targeted compiler build, whitespace formatting and diff checks
pass. No RavenDoc/site generation or native execution was run. Complete generated
runtime analysis continues separately; this correction is ready to commit.

## PORT-079 — qualified constructor source ownership

The reduced remaining list-construction error is not a translation issue. A private
static field's declared type resolves to source `InternalSyntax.SyntaxList`, but
its qualified constructor initializer can select inaccessible bootstrap metadata
while declarations are incomplete. A cold constructor symbol query first avoids
the error. The focused baseline passes **59 tests**; new query-order, tree-order
and relative/full-qualification coverage reproduces **four diagnostic-first
failures**, while **four symbol-first cases pass**.

Canonical namespace expression lookup now admits the indexed source declaration
before accepting a metadata result during declaration binding.
Ordinary metadata-only provider selection remains unchanged. Both targets use the
shared binder path; the Raven binder has not yet been translated. New fixtures
also emit and execute the selected source constructor. Post-change validation is
running; unfinished runtime translations remain unstaged.

Tracing found a second declaration pass: field-initializer completion still runs
inside reentrant source setup after namespace-lookup suppression has ended. A
suppression-only change was insufficient (all eight cases then reported the stale
accessibility error). The correction now admits the indexed source declaration
before metadata selection in either pass. Temporary tracing was removed; focused
and broader verification follow the corrected implementation.

The non-generic correction passes **67 focused tests**, including all eight
source-constructor runtime cases. An additional generic reduction finds that
name-only indexed admission can choose a sibling source declaration with a
different arity and then fall back to constrained bootstrap metadata. Indexed
admission now optionally filters arity before signature completion; the namespace
expression helper uses it for both exact and uniquely inferred generic lookup.
Two regression cases vary source generic/non-generic declaration order. Final
validation is running against this strengthened correction.

Final validation passes **69 focused tests** and **621 broader lookup, alias,
constructor, generic-method, incremental-binder and target-contract tests** on
modern .NET. The final **70-file analysis reports seven errors**, down from nine;
the two qualified list-constructor ownership errors are gone. Targeted builds,
whitespace formatting and diff checks pass. .NET remains default. Native neoCLR
execution and RavenDoc/site generation were not run. The 1,051-file generated
runtime analysis remains live against the PORT-078 seed; managed stack snapshots
confirm ongoing method/invocation binding. It must be inspected on completion
before claiming any complete generated-runtime validation.

## PORT-080 — syntax rewriter token/trivia identity

The larger generated-runtime analysis is terminal: **1,051 inputs** produce
**2,524 errors**. This expands coverage beyond the previous 70-file subset and is
not a regression count. Its seed is PORT-078, before the constructor ownership
fix. Input hashes are saved in
`/tmp/node-runtime-full-generated-analysis-arrays-inputs.json`. The runner also
mistakenly excluded `Diagnostics/generated/CompilerDiagnostics.g.rvn` along with
the syntax outputs being replaced; 29 inaccessible catalog diagnostics are input
artifacts. Future runs must replace only syntax-generated directories and retain
the diagnostic catalog. No complete generated-runtime validation is claimed.

The main actionable groups are missing `GreenNode` qualification in generated
green declarations, required-child null projections in constructors/rewriters,
and missing authored red token factories. Generated fixture runtimes had hidden
some production namespace/nullability differences. Align those fixtures and both
emitter implementations before repeating the full runtime analysis.

Separately, inspection found two unfinished base rewriter methods: nullable token
visits always returned null, and trivia visits always returned default. A
**32-test baseline** passes; all **three new identity/dispatch tests fail** before
the fix. C# and the unfinished Raven rewriter now preserve trivia and dispatch
present nullable tokens to the nonnullable override, retaining absent tokens and
the distinction between absent and present-default tokens. The Raven syntax
runtime remains uncommitted until its dependency closure compiles; this is not
Raven-runtime execution evidence. The first post-change focused run passes
**35 tests**. Broader formatting/code-fix/replacement checks are running.

Final validation passes **184 adjacent formatter, code-fix, replacement,
annotation and rewriter tests** on modern .NET. Targeted build, whitespace
formatting and diff checks pass. The rule is target-independent syntax behavior;
no target policy, source grammar or CLI metadata encoding changes. Native runtime
execution, full Raven runtime execution and RavenDoc/site generation were not run.

## PORT-081 — nullable reference casts retain C# semantics

Author direction reiterates that direct translation should retain C# behavior;
fix missing foundational semantics rather than adjusting ported expressions.
The generator runtime probe's nullable cast failures reduce to `(Leaf)value`
from `Node?`, `(Node)value` from `Node?` and `(Leaf)value` from `object?`. These
ordinary reference casts were rejected, although nullable class-constrained
parameter casts already worked. No deliberate Raven rule requires this gap.

A **78-test baseline** passes. New semantic/cold-query/runtime tests reproduce
**nine failures**, with four control cases passing. Shared conversion
classification now consults the underlying reference relationship and returns an
explicit reference conversion for annotation narrowing. A preexisting shortcut
also implicitly accepted `Leaf?` as nonnullable `object`; it now follows Raven's
existing explicit null-handling rule. Unrelated-class casts remain invalid.
Runtime tests compare null, identity and invalid-type results with an equivalent
C# cast. The generated cast expressions remain unchanged; no suppression
workaround was introduced. Both target frontends use this shared classifier;
its Raven translation is still a later dependency.

**91 focused tests pass** after the correction. Broader nullable/conversion/
overload checks are running. A new complete-syntax analysis uses the corrected
seed and includes the previously omitted diagnostic catalog (**1,052 inputs**).
Its input hashes are `/tmp/node-runtime-full-generated-casts-inputs.json`; the
process must finish before a new full-source error count can be claimed.
The generator production-contract baseline passes **21 C# / 21 Raven tests**;
no generator input or emitter code has changed in this slice.

The broader run passes **864 nullable, conversion, overload, generic-constraint and
target-profile tests**. A follow-up nullable alias case exposes a missing alias
marker in the new conversion path; the classifier now retains that marker.
**149 final focused/alias/API/runtime tests pass**, including conversion-operation
shape and cold type queries, after the marker correction. The in-flight complete
source analysis uses the pre-marker seed; it is not final whole-runtime validation.
No grammar or highlighting changes are needed for existing cast syntax. Modern
.NET execution is validated; native neoCLR guest execution and RavenDoc/site
generation were not performed.

A reduced follow-up also confirms that dotted child namespaces fail to see types
in their enclosing namespace (`Outer.Inner` cannot find `Outer.Root`). This may
explain the generated `GreenNode` lookup group. Investigate the shared namespace
lookup path against C# behavior before adding parent-namespace aliases to emitted
Raven code. No generator changes have been made to mask it.

## PORT-082 — enclosing namespace lookup and .NET lexical scope

The PORT-081 full generated-runtime probe is terminal: **1,052 inputs** produce
**1,573 errors**, with observed conversion diagnostics reduced from 971 to 51.
Inputs include the diagnostic catalog omitted from the prior run; the aggregate
count is not an identical-input comparison. Generated cast expressions were not
changed to hide the compiler gap. The remaining GreenNode lookup group reduces
to a dotted child namespace failing to see a type in its enclosing namespace.

A **73-test namespace/import baseline** passes. Reduced source, framework, relative
qualification and precedence tests then reproduce **13 failures**, with **two
control cases passing**. The new scope-local lookup mode prevents premature
global fallback. NamespaceBinder walks implicit enclosing namespace levels, then
returns to the parent binder at its namespace boundary so lexical import precedence
is retained. Explicit qualified segments remain direct member lookup. **88 focused
checks pass**, including all 15 new cases and execution of a framework Type return.
Workspace edit/reuse coverage and broader baseline validation follow.

Author direction: Raven is a first-class .NET language. Common .NET/C# foundational
behavior is the default, and native neoCLR should retain it when only runtime
encoding differs. Diagnose compiler gaps before inserting translation workarounds;
retain only deliberate Raven departures.

The proposed inline control-flow review is **deferred until the direct port is
stable**: examples include `if x > 10 return true`, expression bodies introduced
by `then`, block bodies, value-producing `if ... then ... else`, and a possible
`while x < 10 do Foo()` form. Current grammar remains authoritative during the
port; this slice changes no if/while parser rule, token or highlighting grammar.

Further validation passes **126 namespace/workspace/incremental checks**, including
renaming and restoring an enclosing source type while the prior compilation stays
valid. The same **1,052-input generated probe falls from 1,573 to 582 errors** after
the namespace correction, removing the GreenNode lookup group without emitted
aliases. Input hashes are `/tmp/node-runtime-full-generated-namespace-inputs.json`.
The broad repository baseline is running from a temporary copy of the standard
script with documentation tests and RavenDoc-related generation disabled; ordinary
compiler XML/.docs sidecars remain enabled for existing metadata fixtures. Its
output is `/tmp/enclosing-namespace-full-baseline.log`; no final baseline success
is claimed while it remains active.

A direct token-primitive translation is now prepared in
`Syntax/SyntaxFactory.rvn` (MissingToken, Token including raw kind, Identifier,
Literal overloads and EndOfFileToken). Parser entry points remain a later
translation, with no stub parser behavior. The **71-file authored corpus parses
with zero errors**. The unfinished runtime remains unstaged; the new complete
probe has **1,053 inputs** and is running in
`/tmp/node-runtime-full-generated-factories.log` against an immutable namespace-fix
seed. Do not claim successful Raven runtime emission from parse-only evidence.

Recovery-node inspection also reduces a preexisting green visitor bug:
`ExpressionSyntax.Missing.Accept` calls `visitor.Visit(this)`, which redispatches
back to Accept. A controlled C# probe stops on the second Visit and reports
**Visits=2, defaults=0** (`/tmp/raven-missing-visitor-proof.log`). Correct both generic
and non-generic dispatch and port the missing-expression nodes after the running
baseline, rather than introducing a Raven-only workaround. The generated red
MemberDeclaration already declares AttributeLists/Modifiers; remaining override
errors therefore need a compiler/declaration-order investigation, not duplicate
handwritten properties.

## PORT-083 — missing-expression recovery dispatch

The broad PORT-082 baseline is terminal: **7,350 passing test executions** include
the C# compiler batches and **300 C# compiler-parity tests**. It stops while building
Raven parity because the unfinished runtime project has **six compile errors**.
This is not an all-green baseline. RavenDoc/site generation was excluded; ordinary
compiler documentation sidecars were produced for fixture prerequisites.

The expanded **1,053-input token-factory probe** is also terminal, with **19 errors**
(down from 582). The factory primitives remain unfinished-runtime work rather
than a claim that the whole Raven syntax runtime executes. Remaining errors are
property override validation, Missing-expression dependencies, the rewriter's
abstract status, nullable token replacement and SyntaxReference ownership.

Two guarded C# tests reproduce recursive green Missing dispatch without allowing
an uncatchable stack overflow. **47 adjacent checks pass / two new cases fail**
before correction; **all 49 pass** afterwards. Both Accept overloads now use
DefaultVisit, returning the generic default result. The corresponding red and
green Missing nodes are translated in Raven with corrected behavior. C# annotations
now reflect nullable parent/green-child storage. The authored corpus parses with
zero errors; full Raven runtime emission is still unverified. No target policy,
grammar or runtime encoding changes.

Ordinary override-chain and source/bootstrap property probes pass in either file
order. A larger signature-only inspection shows correctly virtual base getters,
while some derived getters lose override flags after validation fails. The remaining
property issue therefore needs further declaration/reentrancy reduction, not new
virtual modifiers or duplicate generated properties. Inspection output is
`/tmp/raven-property-signature-proof.log`.

Final validation: **343 adjacent recovery, missing-node, incremental syntax-tree,
rewriter and syntax-contract tests pass** on modern .NET. Targeted build,
whitespace formatting and diff checks pass. The C# correction is ready to commit;
the mirrored Raven nodes remain in the unfinished runtime source batch until
its dependency closure compiles and executes. No native guest execution or
RavenDoc/site generation was performed for this correction.

## PORT-084 — preliminary property inheritance contracts

The production syntax runtime exposes query-order-dependent property override
failures across partial generic bases. Admitting the requested base signature,
as method override lookup already does, is necessary but insufficient: the first
broader run still reports one failing property case (**256 pass / one fail**).
Tracing identifies preliminary accessors with false virtual/override flags before
full binding later corrects them. Validation can retain a bogus RAV0307/RAV0331
from that reentrant observation.

Five direct preliminary-signature tests fail before correction while four
query-order cases pass in that run. Preliminary accessors now preserve declared
abstract/virtual/override/final flags, including implicit abstract interface
properties. The SourceMethodSymbol constructor already normalizes abstract and
override methods to virtual, so no method-specific normalization is added.
Property candidate lookup also admits later partial source signatures. All **11
focused tests pass**, including the existing two method override-order cases.
Temporary trace instrumentation is removed. The signature pass itself is not yet
translated; no Raven-only workaround or generated-property duplication is added.
Control-flow syntax redesign remains deferred while the direct port stabilizes.

Adjacent semantic/incremental validation reports **442 pass / one fail**. The
failure is the same existing reversed-tree method override case seen before this
slice (**181 pass / one fail** baseline), while the property regressions pass in
the broader run. It remains a separate query-order investigation, not a clean
baseline claim. Modern .NET host validation does not imply native execution.

The 1,055-input analysis using the earlier admission-only compiler is terminal
with **19 errors**: 12 property override failures, two implicit base-constructor
failures in the new Missing partial declarations, two apparent abstract-type
instantiations, token replacement overload/nullability and SyntaxReference source
ownership. Its manifest is `/tmp/node-runtime-full-property-admission-inputs.json`;
log `/tmp/node-runtime-full-property-admission.log`. This predates the accessor
modifier correction and is not evidence of its full-runtime result.

All **12 isolated adjacent runtime/accessor/override checks pass** on modern .NET.
Targeted build, whitespace formatting and diff checks pass. No native guest
execution or RavenDoc/site generation is performed. The unfinished Raven runtime
source batch remains separate from this validated compiler correction.

## PORT-085 — constructor admission across partial declarations

The new Missing-expression partials expose another shared gap. A minimal ordinary
class repro has a parameterized base, an explicit constructor forwarding to it in
one part, and an empty second part. The empty part invents a default constructor
and reports RAV1501 for a zero-argument base call. Four C# regression cases fail
before correction, covering explicit/primary constructors in either tree order.

ClassDeclarationBinder now checks all declaring parts for primary and explicit
instance constructors before synthesizing a default. The fix remains local to
constructor admission and preserves existing constructor initializer validation.
Two static-constructor controls confirm that a static constructor alone still
permits the implicit instance default. **All 78 focused/adjacent semantic checks
pass**, including missing-base diagnostics and source constructor ownership.
The Raven binder has not yet been translated; its future direct translation must
retain this combined-declaration contract. No target-specific workaround is added.

The PORT-084 method trace rerun passes **all 443 semantic/incremental tests**;
all observed VisitSample return/parameter matches are valid. This does not close
the intermittent untraced reversed-tree failure. Trace instrumentation is removed.
The expanded syntax-runtime analysis is live using the immutable PORT-084 seed,
manifest `/tmp/node-runtime-full-property-modifiers-inputs.json` and log
`/tmp/node-runtime-full-property-modifiers.log`. It excludes this constructor fix;
do not claim a full-runtime result until the process is terminal.

Final PORT-085 validation: **45 adjacent constructor runtime tests and four new
.NET execution cases pass**. The new cases instantiate the combined partial type,
verify its sole public parameterized constructor, and observe the base's supplied
value for explicit and primary constructors in either tree order. Together with
the 78 semantic checks, these validate the local correction without claiming full
Raven runtime emission. Targeted build, whitespace formatting and diff checks pass.
No native guest execution or RavenDoc/site generation is performed.

## PORT-086 — external location value identity

Dependency work for the source-owned Raven SyntaxTree requires porting locations,
diagnostics and parse options. A focused C# API regression identifies an existing
external-location defect: the constructor stores textSpan in an unused private
field, leaving SourceSpan as `[0..0]`; equality also ignores the external path and
line span. The new regression fails before correction (**22 existing pass**) and
checks span preservation, source shape, equivalent hashes and distinct file,
source-span and line-span identities.

ExternalFileLocation now initializes SourceSpan directly. External Location
equality/hash include the file/line span while source-tree and metadata equality
remain unchanged. Both corrections are mirrored in the pending Raven location
translation. **All 25 focused/adjacent location, diagnostic equality and syntax-tree
contract checks pass** on modern .NET. The new Raven dependency files parse with
zero errors, but full semantic/runtime validation is not yet established. No
native guest execution or RavenDoc/site generation is performed.

## PORT-087 — source-owned syntax-tree runtime foundation

Split C# SyntaxTree runtime responsibilities from parsing and incremental parsing.
SyntaxTree.cs retains storage, creation, diagnostics, locations and navigation;
SyntaxTree.Parsing.cs contains the moved parser/fallback members and parse
factories. The public type/API and method bodies are preserved. **67 scoped tests
pass before and after the split**, followed by **170 adjacent checks**; the final
runtime-foundation build and broader **190 checks** also pass. SyntaxReplacer's
C# green-token key annotation now admits nullable token greens, matching its
existing default-token representation without changing runtime behavior.

Translate the runtime partial, CompilationUnit tree attachment, Diagnostic,
ParseOptions and all Location implementations directly into Raven alongside C#.
Restore the Lexer ParseOptions constructor now that its source dependencies exist.
Location retains explicit equality and the PORT-086 correction. Diagnostic's
normalized message/property equality is preserved rather than replaced with
record synthesis. SyntaxTree retains reference identity and ParseOptions retains
its configuration/snapshot API. Fields and nonpublic helpers remain explicitly
private/internal/protected as appropriate.

The earlier 1,055-input PORT-084 analysis is terminal with **seven errors**, down
from 19: all 12 property override errors disappear. This run predates PORT-085
constructor admission and the new runtime dependencies. Its manifest/log are
`/tmp/node-runtime-full-property-modifiers-inputs.json` and
`/tmp/node-runtime-full-property-modifiers.log`.

Two of those errors also expose translation mistakes: C# sealed classes were
spelled sealed in SyntaxReplacer and the red Missing node. Raven sealed classes
are deliberately abstract hierarchy roots; ordinary Raven classes are closed by
default. Remove those two modifiers to preserve C# concrete sealed behavior.
The source corpus contains no other authored sealed-class translations. This
follows the existing inheritance spec; no language/target policy is changed.

The first 1,065-input source-ownership probe is deliberately terminated after
identifying those incorrect modifiers, not because of elapsed time. Its result
is **superseded, not passed**. The corrected probe uses the immutable seed
`/tmp/raven-syntax-runtime-owner-seed`, manifest
`/tmp/node-runtime-source-owner-closed-inputs.json` and log
`/tmp/node-runtime-source-owner-closed.log`. It remains live at this checkpoint.
All authored sources parse with zero errors, but semantic compilation, emission
and shared C# tests against this expanded Raven runtime remain unproven. Keep the
pending Raven batch out of validated-slice commits until its closure works. Modern
.NET host validation is not native guest execution. No RavenDoc/site generation.

## PORT-088 — init-property construction contexts

The 1,065-input source-owned runtime probe is terminal with **21 errors**. All
seven errors from the preceding probe disappear: partial constructor admission,
concrete class spelling, nullable token keys/casts and tree-reference identity now
resolve. New Diagnostic/ParseOptions/SyntaxTree dependencies expose additional
inference, construction, initialization and translation gaps; this is not a green
Raven runtime build. Manifest/log are `/tmp/node-runtime-source-owner-closed-inputs.json`
and `/tmp/node-runtime-source-owner-closed.log`.

A separate source symbol probe passes: SyntaxReference.GetSyntax, SyntaxTree.GetRoot,
SourceLocation.SourceTree and Diagnostic.Location all retain Raven assembly-owned
return types. Red Missing and SyntaxReplacer are concrete/closed; green Missing is
concrete/open. These are signature checks, not emission or parity-test evidence.

Init-property assignment in ParseOptions constructors contradicts the existing
language spec and C# construction semantics: BlockBinder allowed init setters only
in object/with initializers. A **60-test feature baseline passes** but contains a
stale test requiring the incorrect constructor diagnostic. Correct that expectation
and retain ordinary-method rejection. After repairing two declaration separators
in initial test fixtures, an immutable pre-fix compiler runs the final regression
set: **10 failures / six rejecting controls pass**. This includes .NET/shared source
cases and neoCLR semantic binding with host references.

Binding now checks the innermost callable and receiver: instance constructors and
init accessors can write current/base properties through implicit self, self or base.
Initializer blocks use constructor ownership. Another instance, ordinary methods,
lambdas and local functions remain rejected. Accessible init setters participate in
assignment, compound updates, increments and pipeline property writes; computed
setters keep their behavior. Shared target code owns this policy, with no duplicate
init-assignment policy in the neoCLR target contracts. The binder is not yet ported;
ParseOptions remains directly translated instead of changing its property contract.

**All 76 focused/adjacent semantic tests pass**, including three explicitly neoCLR
semantic cases. **All 43 adjacent property/runtime tests pass**, including three new
.NET executions of custom init setter side effects and inherited C# metadata init
properties. Targeted build passes. No native guest/core execution or RavenDoc/site
generation is claimed. Remaining runtime gaps will be reduced independently.

## PORT-089 — preserve imported init accessor classification

Inspection while reducing metadata value construction identifies a second init
interop defect: PEMethodSymbol detects IsExternalInit, then unconditionally assigns
PropertySet. The PORT-088 metadata constructor execution therefore passed without
proving that later writes were restricted. A **38-test baseline passes**; new
metadata shape, ordinary update and receiver cases produce **seven failures and
five control passes** before correction.

Initialize ordinary setter kind before detecting the required modifier, retaining
InitOnly when present. No new accessor encoding or target policy is introduced.
The PE symbol component has not been ported; preserve this classification in its
future Raven translation. Existing source ParseOptions assignments retain the
PORT-088 binding correction.

**All 53 focused cases pass**, covering metadata shape, assignment/compound/
increment/pipeline rejection, mutable setter controls, object initialization,
self/base construction and foreign-receiver rejection. They include five explicit
neoCLR semantic cases using host references, not a native core/runtime. **16
adjacent record-copy, required-member and accessor interop checks pass** on modern
.NET. Targeted build, whitespace formatting and diff checks pass. No native guest execution or RavenDoc/site generation.

The focused pending Diagnostic/ParseOptions analysis after PORT-088 has **eight
errors**, down from 16: init-property constructor assignments are admitted; enum
name binding, conditional collection inference, lambda return inference, implicit
metadata value construction and a nullable comparer overload remain. The full
source-runtime probe is still the older 21-error run and is not a current emission
or parity-test result. Resolve these reduced gaps before another full run.

## PORT-090 — implicit metadata value construction

Diagnostic's direct `HashCode()` translation exposes absent implicit metadata value
constructors. The **22-test constructor/symbol baseline passes**, but five new
constructor symbol/operation cases fail for HashCode, Guid, Int32, DateTime and
ValueTuple<int>. PE metadata has no actual zero-argument method for these defaults.
Synthesizing a real method reference or rewriting the Raven algorithm to default
syntax would lose the intended construction contract.

Add a canonical synthesized constructor symbol to PE value types without a declared
instance parameterless constructor, excluding physical System.Void. It is an
identity-bearing symbol class, not a record value. All/named member enumeration,
Constructors, symbol queries and object-creation operations retain the constructor;
only execution lowers to default value initialization. Explicit constructors and
object initializers retain their ordinary behavior. Shared linear lowering checks
the existing DefaultValue capability and excludes fake external constructor calls.
The PE/symbol/lowering components are not yet translated to Raven; preserve this
contract when porting them. The pending Diagnostic translation stays direct.

The generic case then identifies distinct substituted constructor instances across
all-member and named queries. Three independent metadata method/property/field
projection tests fail against an immutable pre-fix compiler. Stable projections
now share a reference-keyed cache, and repeated stable all-member calls avoid
rebuilding discarded arrays. Mutable source declaration states and source unions
keep their existing uncached path. No caller-side semantic cache API is introduced.

**All 128 final adjacent constructor, constructed-symbol and incremental checks
pass**, including two explicit neoCLR semantic cases with host references. An
earlier run reproduces the known intermittent reversed-tree method override failure;
it remains open rather than being declared fixed by the later passing run.
The **439-test overload-resolution feature suite passes**. A .NET execution fixture
compares seven construction paths with C# results: generic zero and argument forms,
an object initializer, an explicit default constructor, an optional-argument
constructor, enum zero and mutable HashCode execution. It passes on modern .NET.
Targeted builds pass; whitespace formatting and diff checks pass. No native guest
execution or RavenDoc/site generation is performed.

The focused pending Diagnostic/ParseOptions analysis now has **seven errors**
(`/tmp/diagnostic-options-value-constructor-fixed.log`): HashCode construction no
longer fails. Remaining reduced gaps are conditional collection inference,
heterogeneous lambda returns, a nullable comparer overload and enum/member name
binding. The full source-runtime build still has no current successful emission
or shared C# parity result; keep its unfinished Raven source batch unstaged.

## PORT-091 — common interface inference

The direct syntax-tree port's `_diagnostics ?? Enumerable.Empty<Diagnostic>()`
loses its generic contract and reports nongeneric IEnumerable instead. A **47-test
inference/coalescing baseline passes**. Eleven focused cases then produce **nine
failures / two controls pass** before correction, covering cold/diagnostic-first
coalescing, interface self-contracts, derived contracts and reference branches.

TypeSymbolNormalization now includes an interface itself in shared-contract checks,
retains the unique most-specific interface and uses class/object fallback for
incomparable survivors. This removes metadata-order-dependent choices. A reference
branch regression exposes a separate class-only inference path and interface
rejection in BlockBinder. Its reference fallback now consults the same normalizer,
retaining valid existing operand conversions. Value-type inference restrictions,
explicit contextual targets and user-defined conversion selection retain their
own paths.

Two older assertions accepted an arbitrary first common interface for mixed returns;
one test's name already described object fallback. They now assert object for
incomparable contracts. **71 focused/adjacent inference, return, analyzer and
coalescing cases pass**, including cold/warm branch type queries and an explicit
neoCLR semantic case using host references. Modern .NET runtime checks preserve
selected branch/coalescing value identity, generic enumeration and fallback
short-circuiting. The normalization/binder have not yet been ported; keep these
rules in their future direct translation. The Raven runtime source remains
uncompiled and unstaged pending its remaining dependency gaps.

Final PORT-091 validation: the complete 16-case regression set produces **14
failures / two controls pass** against the immutable pre-fix compiler. All cases
pass with the correction as part of the 71-case adjacent run. The **439-test
overload-resolution feature suite passes**, and the .NET execution fixture verifies
both conditional branch identities as well as coalescing identity/laziness.
Targeted builds, whitespace formatting and diff checks pass. Ordinary compiler
fixture documentation sidecars were produced by the feature build; RavenDoc/site
generation was not run. Native guest execution remains unqualified.

## PORT-092 — local tree roots for direct translation

The pending SyntaxTree translation reassigns its compilationUnit parameters in
two factories. Raven parameters are immutable; this is a translation boundary,
not a reason to change parameter semantics. Use a local root after WithSyntaxTree
in both C# factories and mirror that local in Raven. Inputs, root attachment,
text snapshot and diagnostics remain unchanged. This is bounded cleanup within
two methods, with no public API or language rule change.

**28 focused tree/rewriter/location checks pass before and after** the C# cleanup.
Targeted build passes; all authored Raven inputs parse with zero errors. The Raven
factory file remains in the pending runtime dependency batch until semantic
compilation and execution are validated. A new 1,065-input analysis uses immutable
seed `/tmp/raven-tree-root-locals-seed`, manifest
`/tmp/node-runtime-tree-root-locals-inputs.json` and log
`/tmp/node-runtime-tree-root-locals.log`; it is live at this checkpoint. It includes
PORT-088/089 init fixes, PORT-090 metadata defaults, PORT-091 generic interface
inference and these locals. Do not infer emission success from its syntax checks.
No native guest execution or RavenDoc/site generation is performed.

## PORT-093 — nullable generic CLI declaration contexts

The pending SyntaxTree runtime uses FirstOrDefault followed by nullable member
access and reassignment. A reduced C# regression fails in both cold and
post-diagnostics query orders: the imported constructed method returns string
rather than string?. The existing 23-test metadata/substitution baseline passes.

ReflectionTypeLoader now uses explicit nullable flags first, then the nearest
member/enclosing-type NullableContextAttribute. Keep the generic-parameter
exclusion in reflection-derived state: removing it incorrectly makes First<T>
nullable too, because NullabilityInfo treats unconstrained T as potentially null
regardless of its declaration annotation. Context flags distinguish those cases
and feed the existing substitution-aware nullable representation. Parameter,
return, field, property and event import use the same declaration-context helper.

**95 focused/adjacent tests pass**, including cold/warm reference results,
First/LastOrDefault/SingleOrDefault, value-result controls, existing nullable
substitution and flow-attribute contracts, plus two neoCLR host-reference semantic
cases. A modern .NET execution fixture matches C# LINQ for empty/nonempty string
and int sequences, including conditional member access after local inference.
NativeAssemblySymbol.MapAnnotatedView already applies explicit nullable flags to
generic uses; it does not consume CLI context attributes. No native guest
execution is claimed. The loader is not yet translated; carry this rule into its
future Raven port rather than annotating around the defect in SyntaxTree.rvn.

The 1,065-input full runtime analysis remains on its earlier immutable seed and
does not contain this correction. Its result cannot qualify PORT-093. No full
runtime emission or parity completion is claimed. No RavenDoc/site generation
was run; the overload-suite build may produce ordinary fixture sidecars.

The first 439-case overload run exposes **four failures / 435 passes**: newly
accurate IEquatable<T>.Equals(T?) import reveals that interface signature matching
only strips reference annotations. An annotation-only substitution for a struct
must also match its underlying physical type. An 11-case abstract-member baseline
passes before updating the existing signature helper to strip annotations based
on ABI projection, preserving genuine Nullable<T> distinctions. **112 focused and
adjacent cases pass** after correction, including the four record-struct failures
and an int versus int? implementation control. This semantic-model helper is also
shared with neoCLR; it has not yet been translated into Raven.

The repeated **439-case overload suite passes** after the semantic signature
correction. Runtime record equality then exposes the same assumption in shared
synthesized method lookup and .NET TypeGenerator interface matching. Both now
ignore annotation-only decorations, preserving genuine nullable-value storage.
The first failure cannot resolve EqualityComparer<int>.Equals(int, int); after
correcting lookup, interface method flags still fail CLR loading until the emitter
comparison receives the same correction. Retain the interface-dispatched record
equality runtime regression to cover the complete chain.

The earlier 1,065-input analysis is now terminal: **nine errors** in
`/tmp/node-runtime-tree-root-locals.log`. Seven are in Diagnostic/ParseOptions
(collection target inference, mixed lambda returns, nullable comparer overload,
and enum/member name resolution); two are the FirstOrDefault local-nullability
errors addressed by this slice. Common-interface inference and tree local-root
errors are absent. This immutable earlier seed does not include PORT-093, so a
fresh expanded compilation still remains necessary; do not report seven as a
measured full-runtime result.

Final validation: **13 runtime/record checks pass**, covering conditional LINQ
reference results, unlifted integer results, record equality through IEquatable,
record copying/inheritance and text value records. Targeted builds, whitespace
formatting and diff checks pass. The .NET metadata suite is explicitly marked
Target=DotNet; the two host-reference neoCLR semantic cases remain separately
marked Target=NeoCLR. No native guest execution or full Raven runtime parity is
claimed. The unfinished Raven runtime batch remains uncommitted.

## PORT-094 — conditional empty-collection context

Diagnostic.GetMessage directly translates a conditional object-array/empty
collection argument to string.Format. A 39-case collection/incremental/target
baseline passes, but four branch-order and cold/warm query cases fail. A local C#
comparison confirms the analogous conditional inherits the other array type.

BlockBinder now binds the independently typed branch first only when the other
branch ends in an empty collection and no contextual target exists. It supplies
that type to the empty branch, preserving explicit contexts and nonempty-literal
defaults. When the else branch is bound first, condition pattern bindings are
removed for that branch and restored for the successful branch. Both-empty
conditionals still require an external target. No syntax, highlighting or public
API is added. Direct collection-node type queries cover editor-facing semantics.

**87 focused/adjacent tests pass**, including independent collection queries,
pattern scope, string.Format overload context, incremental collection coverage,
common interfaces and two neoCLR semantic cases with host references. A modern
.NET runtime fixture proves selected-branch execution and array identity in both
orders. The language-reference sources are also the website sources selected by
docs/ravendoc.json; no RavenDoc/site generation is run.

The reduced five-source Diagnostic/ParseOptions analysis now reports **five
errors** in `/tmp/diagnostic-options-conditional-fixed.log`, using immutable seed
`/tmp/raven-conditional-collection-seed`: mixed lambda returns (two), nullable
comparer overload (one), and enum/member resolution (two). Both conditional-array
errors are gone. This reduction deliberately uses bootstrap Location and is not
full source-runtime ownership or emission qualification. BlockBinder is not yet
ported; carry this behavior into its future Raven translation. Native guest
execution remains unverified, and the unfinished Raven runtime batch stays
uncommitted pending its remaining dependencies.

Final PORT-094 validation: **439 overload-resolution tests pass**. Targeted
compiler/test builds, whitespace formatting and diff checks pass. The runtime
fixture passes both branch orders, including empty results without fallback
calls and identity-preserving selection with exactly two total calls. No full
1,065-input reanalysis or Raven-runtime emission has been claimed for this seed.

## PORT-095 — retain all common-type operands

The direct Diagnostic.ProcessArgs translation returns a display string for a
symbol and the original object? otherwise. Two cold/warm LINQ Select regressions
fail despite a passing 59-case lambda/early-return baseline. The defect is shared
normalization, not lambda-specific: FindCommonNominalType silently filters out
NullableTypeSymbol and arrays using OfType<INamedTypeSymbol>, then infers from the
remaining string alone. A 27-case common-type/interface baseline passes; four
new nullable/array order controls fail before correction.

TypeSymbolNormalization now unwraps nullable reference operands for the nominal
search, preserves nullable reference results, and declines the nominal search
when it cannot represent every operand. Existing class/interface selection and
object fallback remain authoritative rather than inferring from a subset. No
lambda workaround is added. Neither the normalizer nor lambda binder has been
ported to Raven yet; carry the shared rule into their eventual direct translation.

**120 focused/adjacent tests pass**, including nullable and array operand order,
cold/warm mixed-return Select, common interfaces, collection inference, early
returns and a neoCLR host-reference semantic case. The language-reference source
is also website input. There is no syntax or grammar change; public symbol/type
queries carry the correction to language-service consumers. Native guest runtime
execution remains unqualified.

The modern .NET execution fixture matches C# LINQ and preserves nulls, original
reference identity and boxed-value identity. The reduced five-source probe using
immutable `/tmp/raven-mixed-return-seed` now reports **four errors** in
`/tmp/diagnostic-options-mixed-return-fixed.log`. The lambda return conversion
error is gone and its inferred signature is object? -> object?, but conditional
access still reports conversion to the indexed Select delegate. That remaining
overload/replay gap is separate and must not be hidden by changing the Raven
translation. Nullable comparer and enum/member errors also remain. The reduced
probe uses bootstrap Location; full Raven runtime ownership/emission/parity is
still unqualified. No RavenDoc/site generation was run.

Final PORT-095 validation: **439 overload-resolution and 189 union feature tests
pass**, in addition to the 120 focused/adjacent checks and runtime fixture.
Targeted builds, whitespace formatting and diff checks pass. The pending Raven
runtime source batch remains uncommitted; no full expanded-runtime success is
claimed from this reduced probe.

## PORT-096 — compatible delegate hints through conditional access

After correcting common return inference, Diagnostic.ProcessArgs still reports
object? -> object? incompatible with indexed Select's two-parameter callback.
A 47-case conditional-access/mixed-return baseline passes. Four new conditional
Select cases fail in cold/warm queries, with and without empty coalescing fallback:
the selected method is already correct, but an incompatible hint leaves a diagnostic.

TryGetFirstDelegateParameterType now considers the argument syntax and skips
concrete delegates with a different explicit lambda parameter count. It keeps
implicit-parameter forms, method groups and unshaped System.Delegate policy on
their existing paths; final overload resolution remains authoritative. The rule
is shared by targets, not a LINQ or nullable-receiver special case. Zero-, one-
and two-parameter source overloads are tested in both declaration orders.

The reduced Diagnostic/ParseOptions probe using immutable seed
`/tmp/raven-conditional-select-seed` reports **three errors** in
`/tmp/diagnostic-options-conditional-select-fixed.log`: nullable HashCode comparer
inference and two enum/member-name failures. Conditional Select now binds without
changing the direct Raven translation. This probe uses bootstrap Location and
still does not qualify full source-owned runtime compilation or emission. The
binder is not yet translated; preserve this hint rule in its eventual Raven port.
Language-reference sources also feed the website. No syntax/highlighting or
Runtime Contract change is needed, and RavenDoc/site generation is not run.

The initial broad run has **438 passes / one failure**: an invalid callback with
no matching signature loses its existing conversion diagnostic. Preserve the
original first delegate as a diagnostic fallback only when no compatible delegate
exists; valid calls still use an arity-compatible hint. System.Delegate's existing
unshaped-handler policy takes precedence over either hint. The focused invalid
callback regression remains authoritative rather than weakening its assertion.

Final focused validation: **151 semantic/adjacent checks and 21 .NET runtime
conditional-access checks pass**, including a separately identified neoCLR
semantic case with host references. Runtime coverage proves single receiver
evaluation, null short-circuiting, projection identity, and indexed callbacks.
The final reduced probe at `/tmp/diagnostic-options-conditional-select-final.log`
still has exactly the three remaining errors using immutable
`/tmp/raven-conditional-select-final-seed`. Targeted builds and formatting pass.
No native guest execution is claimed.

A fresh **1,065-input full analysis** is running from that final seed with input
hashes in `/tmp/node-runtime-conditional-select-inputs.json` and output in
`/tmp/node-runtime-conditional-select.log`. It includes PORT-093 through PORT-096
and is not yet evidence of full runtime compilation or emission success. Keep the
unfinished Raven source batch uncommitted while its dependency gaps remain.

The final **439-test overload-resolution suite passes** with diagnostic fallback
retained. Whitespace formatting and diff checks pass. The full analysis remains
live at this checkpoint; its future result must be inspected independently.

## PORT-097 — core JSON consumers of accurate nullable imports

The nullable-comparer investigation establishes a separate metadata gap:
StringComparer implements nullable-string generic comparer interfaces, with
NullableAttribute transforms stored on InterfaceImpl rows. Reflection-only
ImplementedInterfaces discards these attributes. Three of four HashCode.Add
controls fail, including explicit string? type arguments; the nonnullable inferred
control passes. Raw metadata inspection in `/tmp/interface-metadata-inspection.log`
confirms interface transforms [0, 2]. MetadataLoadContext.Module.ResolveType throws
NotSupportedException, so a future correction needs target-owned image/declaration
facts rather than token resolution through reflection or weakening nullable variance.
The new comparer regressions remain pending and uncommitted.

A broader baseline was started before changing metadata loading, using a temporary
copy of scripts/test-baseline.sh that excludes the known failing comparer repros
and RavenDoc/site-generation classes. It stops during Raven.Core compilation,
before test batches: three converter assignments reject newly accurate T? results
from JsonSerializer.Deserialize<T>. `/tmp/interface-metadata-baseline.log` records
this failed prerequisite; do not call it a green broad baseline.

The three converter locals now explicitly coalesce to default(T)/default(E).
This preserves null reference payloads and generic default storage, without
asserting non-null or changing Optional's JSON-null-to-None rule. Source changes
are local to Read methods. Raven.Core is already Raven-only; there is no C#
implementation counterpart. C# regressions cover both Result tags with null
payloads and Optional Some with a custom converter producing null from a non-null
JSON token. **27 Optional/Result tests pass on .NET 10**, and the .NET 11 library
build passes. No native guest JSON execution is claimed.

The full 1,065-input analysis is terminal with **four errors** in
`/tmp/node-runtime-conditional-select.log`: the known comparer and two enum/member
errors, plus SyntaxTree's IEnumerable<DiagnosticInfo>? coalesced with an array
falling back to object. The latter is a remaining common-inference gap: preserve
all operands while using arrays' actual base/interface contracts. Earlier
FirstOrDefault and conditional/mixed-lambda errors are absent. The runtime still
has no successful full compilation/emission/parity result.

Nine adjacent union tests also pass on .NET 10. Whitespace formatting completes
(with workspace-loading warnings), and diff checks pass. Ordinary compiler
library documentation sidecars are emitted by the builds; RavenDoc/site generation
is not run. The final comparer repro set has **four failures / one passing control**,
including a direct imported-interface nullability assertion. No metadata-loader
correction has been committed yet.

## PORT-098 — array contracts in common-type inference

The pending SyntaxTree translation coalesces IEnumerable<DiagnosticInfo>? with
Array.Empty<DiagnosticInfo>(). The previous conservative common-type search kept
array operands but rejected nominal inference whenever any operand was not a named
type. That lost arrays' existing base/interface contracts and inferred object.

The shared normalization now consults ITypeSymbol contracts for every operand,
starting non-named operands at their nominal base. No operand is discarded, and
unique-interface selection and class/object fallbacks remain in place. Target
array providers still own the applicable interface facts. The normalization has
not yet been ported to Raven; this bootstrap correction enables the direct Raven
source without introducing a workaround there. No syntax, TextMate grammar,
language-server-specific path or generated model changes are required.

The pre-change focused baseline passes 34 tests. Six new cases fail before the
fix: both coalescing orders under cold/warm .NET semantic queries and two neoCLR
semantic cases with host references. After the correction, 63 focused semantic
tests and five separately run .NET execution tests pass. The execution cases
verify reference identity, generic values and fallback short-circuiting. These
are modern .NET tests, not native neoCLR guest execution.

The broader pre-change baseline after PORT-097 stops in
AttributedCustomUnionTests.TypedCaseCarrierLoadsAsUnionWithoutBoxedValue on a
documentation-link assertion (Choice/index.html versus Choice/method__ctor.html).
This remains a separate recorded failure; no full baseline success is claimed.
The nullable-comparer metadata regressions and unfinished runtime files remain
outside this slice.

The 439-test overload-resolution suite and 189-test union suite also pass.
Whitespace formatting and diff checks pass. Full runtime analysis is running
against an immutable updated compiler seed with all 1,065 input hashes verified
against the previous run; its result is recorded separately when terminal.

A reduced enum/property-name repro in /tmp/parse-options-enum-probe.rvn also
reproduces both ParseOptions errors with the enum declaration present. This is
independent of the array correction and remains the next focused binding gap.

## PORT-099 — same-named enum receiver binding

ParseOptions uses DocumentationFormat both as an enum type name and as a
property. A standalone reduced declaration reproduces the constructor assignment
and optional-default failures with the enum present. C# permits the enum type
meaning in this case; the bootstrap previously bound the value and searched only
instance members. A 23-test enum/default/type-resolution baseline passes, while
seven new cases fail and the differently typed shadow control passes.

Shared member binding now recognizes matching simple enum type/value names and
selects a type receiver for enum constants before evaluating the value. The
correction is scoped to enum constants; it does not claim implementation of every
C# identical-name rule for arbitrary static/instance method overload groups.
Qualified values and differently typed shadows retain ordinary lookup. Parameter
default evaluation uses the same binding path. Both member and receiver symbol
queries are covered, including receiver-first cold queries and queries after
diagnostics. The binder has not yet been ported, so the direct Raven ParseOptions
source requires no workaround or duplicate binder edit.

The focused semantic set passes 32 tests, and all nine collision cases pass again
with receiver-first queries. Fifteen separately run .NET execution tests pass,
including a getter counter and reflected optional enum default. Two of the
semantic cases select neoCLR with host references; they query semantic-model
diagnostics rather than validating an unconfigured native core. No guest
execution is claimed. Specifications, compiler API notes and the C# transition
website source document the enum rule; no grammar, TextMate or generator change
is needed, and no RavenDoc/site generation is run.

The 439-test overload-resolution suite passes. The standalone reduced case now
passes driver analysis against /tmp/raven-enum-name-seed. Whitespace formatting
and diff checks pass. The prior full runtime analysis remains live at this
checkpoint (session 40357, /tmp/node-runtime-array-contract.log); it uses the
PORT-098 compiler and must be assessed independently before a new full run.

## PORT-100 — nullable CLI interface declarations

PORT-098 full analysis is terminal: 1,065 verified inputs produce three errors
in /tmp/node-runtime-array-contract.log. SyntaxTree's array/enumerable error is
gone; the comparer call and two enum-name errors remain in that older compiler.
PORT-099 separately fixes the reduced enum case.

InterfaceImpl NullableAttribute transforms cannot be recovered from reflection's
ImplementedInterfaces enumeration. The .NET metadata session now captures each
selected module's metadata bytes at load time, caches declaration facts without
Compilation symbols, and decodes interface signatures through the selected
MetadataLoadContext. Assembly scope, nested types and constructed arguments remain
explicit. Projection neither reopens paths nor matches unqualified names or
reflection order. The additional snapshot retains metadata only, not full PE
images. Direct Interfaces uses declared rows; AllInterfaces retains transitive
interface and base-class contracts. The existing nullable-transform projector
applies row flags, or the nearest enclosing nullable context when absent.

A 39-test metadata baseline passes before edits. The final focused set passes
68 tests, including nested generic/array annotations, generic substitution,
inherited nullable interfaces, a nonnullable control and a deleted-reference-path
snapshot check. Two .NET runtime tests compare null/non-null HashCode results
against C#. The adjacent metadata/reference suite passes 101 tests; all 439
overload-resolution tests pass. One neoCLR semantic case uses host references.
No native guest execution is claimed. Native interface-edge views currently lack
an annotation projection contract; this is documented as a remaining gap in the
CLI bridge notes, not a target language rule.

Accurate imported annotations expose a separate shared inference defect:
HashCode.Add with a non-null string and StringComparer infers string? in Raven,
while a Roslyn oracle infers string. Nullable and explicit-type cases agree.
The metadata acceptance tests assert binding and explicit/nullable uses; the
separate ContravariantNullableInferenceTests oracle regression remains uncommitted
and failing for the non-null inferred case. Do not call overall semantic parity
complete. OverloadResolver currently merges all generic argument bounds without
respecting parameter variance; fix that as the next inference slice rather than
weakening metadata or nullable conversion rules.

The metadata loader is not yet ported; this bootstrap correction enables the
unchanged Raven Diagnostic source. Docs/changelog record the import and API
behavior. Formatting and diff checks pass. A new full 1,065-input driver analysis
is active in session 60859, /tmp/node-runtime-interface-metadata.log, using the
immutable /tmp/raven-interface-metadata-seed with PORT-099 and this importer.
Its result is not yet known, and full compilation/emission/parity remains open.

The compiler library builds for both net11.0 and net10.0. The tests above execute
on modern .NET 11; the net10.0 result is a build check only. The isolated Roslyn
oracle set is terminal with one failure and three passing cases in
/tmp/contravariant-nullable-before.log, confirming the next task independently.

## PORT-101 — variance-aware inference and contextual callback queries

The PORT-100 full driver analysis is terminal with two errors in the unchanged
Raven ParseOptions translation: dictionary-copy overload selection and a nullable
string-array result from its normalization pipeline. The preceding comparer and
enum diagnostics are gone. Accurate imported nullable interfaces exposed shared
generic inference that treated every argument as a widening lower bound.

Inference now retains candidate-local lower, upper and exact bounds, composes
declared variance (including nested contravariance), and fixes the candidate from
all evidence. Reference annotations follow exact/lower evidence before upper-only
evidence. Method-group trials copy bounds along with provisional substitutions.
Contextual callback placeholders equal to the inference variable contribute no
evidence; this preserves the existing nullable property-builder callback case.
No comparer-name or target-specific workaround is introduced.

The reduced ParseOptions source also exposed a cold semantic-query defect. A
callback query selected the enclosing function declaration as its rebind root,
which registered a signature without binding the arrow body. Contextual roots
now stop at function/method declarations and bind executable expressions under
the existing re-entry guard. Candidate-only callback failures receive contextual
binding before being treated as final. Forward/reverse query order, repeated
queries and diagnostics-first queries agree in the reduced port fixture.

Validation on modern .NET 11:

- 439 overload-resolution tests pass, including the property-builder regression.
- 137 adjacent semantic-query/caching/callback tests pass. Four stale expected
  JsonSerializer result annotations now reflect the nullable metadata return
  already imported by PORT-093; input argument annotations are unchanged.
- 147 focused inference tests pass, including Roslyn comparisons of nullable and
  nominal arguments in either parameter order, nested contravariance, covariance,
  invariance, and all four ParseOptions query-order cases. These sets overlap.
- Five isolated execution tests pass, including C# comparisons of selected generic
  runtime types, nullable comparer hashing and adjacent Select behavior.
- Compiler library builds pass for net11.0 and net10.0; net10.0 is a build check.
  Formatting and diff checks pass.

Two neoCLR semantic cases use host references. Native execution, .NET Framework
and NanoFramework are not qualified by these results. The native nullable
interface-edge projection gap remains documented in the CLI bridge notes. The
inference and semantic-model components are not yet translated; these C# fixes
enable the Raven source without changing its algorithm. Language/API docs, the
C# developer guide and changelog are updated; no syntax or highlighting changes
are required, and RavenDoc/site generation was not run.

The full 1,065-input analysis is running in session 28721 using immutable
/tmp/raven-variance-inference-seed; source hashes match
/tmp/node-runtime-conditional-select-inputs.json. Its log is
/tmp/node-runtime-variance-inference.log. Its result is not yet known. Keep the
pending Raven syntax-runtime batch uncommitted until full compilation, emission
and parity qualify it; this slice only commits the independently validated
bootstrap fixes. The unrelated broad-baseline documentation-link failure recorded
in PORT-098 remains unresolved, so no broad green baseline is claimed.

## PORT-102 — ParseOptions execution contracts

While full syntax-runtime analysis continues, the actual pending ParseOptions.rvn
source now emits independently with the PORT-101 seed and bootstrap diagnostic
and documentation-format dependencies. A reflection comparison first confirms
defaults, normalization, snapshot isolation, equivalence and copying. Five new
C# ParseOptionsContractTests lock those behaviors without depending on parsing.
The adjacent pre-change options/directive baseline passes 11 tests; adding the
contracts yields 16 passing C# tests.

The exact test bodies also pass against the emitted Raven component (five tests)
in /tmp/parseoptions-parity-tests. The temporary consumer only adds aliases for
ParseOptions and SourceCodeKind to select Raven types; all assertions and inputs
are unchanged. The component is emitted from the maintained ParseOptions.rvn and
AssemblyInfo.rvn sources, preserving its normal friend-assembly access. Commands
use /tmp/raven-variance-inference-seed/rvnc.dll with --framework net11.0,
--output-type classlib, --emit-core-types-only and a reference to that seed's
Raven.CodeAnalysis.dll. Logs: /tmp/ported-parse-options-friend-emit.log and
/tmp/parse-options-raven-contract-tests.log.

This is a reduced execution probe, not full Raven source ownership: Diagnostic
and DocumentationFormat still come from the C# bootstrap in this probe. The
Raven source and generator/runtime batch remain pending, and the shared parity
project will link these tests when that complete dependency closure qualifies.
The compiler reports existing default-initialization and null-suppression warnings
in this source; emission succeeds and the tested default values match C#.
No C# or Raven implementation algorithm was changed in this test slice.

The full 1,065-input analysis remains live in session 28721, with PID 62222
confirmed active after six minutes. Do not launch a replacement based on its
quiet log. Its terminal result, full emission and source-owned parity are still
required before committing the pending syntax-runtime translation.

## PORT-103 — accessible method and field hiding candidates

The PORT-101 full 1,065-input analysis is terminal and successful: zero errors,
1,817 warnings, 759.6 seconds. Source hashes match the original manifest. This
clears the dictionary-copy and normalization diagnostics from PORT-100. The
warning inventory contains 1,664 explicit null-suppression warnings and twelve
member-hiding warnings, including generated private cache fields.

Those cache warnings reduce to a shared binder defect: method/field candidate
search ignored accessibility. A three-test baseline passes. New Roslyn-oracle
private/protected/internal/public cases and a neoCLR host-reference case expose
two failures before the fix (six controls pass). Candidate search now uses the
existing AccessibilityUtilities rule from the declaring type and continues past
inaccessible members. No field renaming or generator workaround is needed.

All 48 focused and adjacent member-hiding, accessibility, inheritance and override
contract tests pass on .NET 11 after the change. Compiler/test builds, formatting
and diff checks pass. The source/interface semantic rule is shared with neoCLR;
its focused check uses host references and claims no native execution. Runtime
encoding and override validation are unchanged. The binder is not yet translated,
so the correction belongs to the C# bootstrap and benefits generated Raven code.
Spec/API docs and changelog describe the diagnostic correction. Existing syntax
and highlighting are unchanged; no RavenDoc/site generation was run.

Full syntax-runtime emission started from the same 1,065 source hashes and
immutable PORT-101 seed in session 39453 (PID 65774), writing
/tmp/node-runtime-variance-emit.log and, on success,
/tmp/Raven.CodeAnalysis.Raven.dll. It deliberately does not include this later
diagnostic-only fix. Do not restart it while active.

The C# side of the prepared expanded parity corpus passes 322 tests using
/tmp/syntax-runtime-parity/CSharp/Probe.csproj. The corresponding Raven consumer
is /tmp/syntax-runtime-parity/Raven/Probe.csproj and must run after successful
emission. It reuses SharedTests.props plus the existing statement/declaration
factory tests and the ParseOptions contracts. Temporary identity checks verify
syntax-runtime assembly ownership and that all emitted types load. The driver
supplies a bootstrap compiler reference; unported IModuleSymbol and
DocumentationFormat dependencies remain explicit limitations. The Raven consumer
aliases bootstrap DocumentationFormat while selecting Raven syntax/runtime types.
Full emission and Raven-side parity are still pending; the runtime source batch
remains uncommitted.

## PORT-104 — abstract properties are storage-free contracts

The successful full analysis reports initialization warnings on generated
abstract syntax-node properties. Inspection shows a compiler defect rather than
a port problem: abstract accessor lists and shorthand abstract properties receive
synthesized backing fields and auto-property classification. The initialization
analyzer then treats the contract as stored state; emitted metadata also contains
the unnecessary field.

A 41-test initialization/override baseline passes. Seventeen new focused cases
fail before the correction: eight cold/warm symbol checks, four analyzer cases,
four emitted-metadata cases and one neoCLR host-reference symbol case. Shared
property binding now excludes abstract members when creating storage. Accessor
creation is unchanged. No analyzer suppression, generated-source workaround or
target-specific implementation is introduced.

After the correction, 116 semantic/analyzer/adjacent property tests and sixteen
isolated metadata/runtime tests pass on .NET 11. The latter cover abstract field
absence plus concrete accessor identity, auto-property stability and constructor
initialization behavior. Compiler/test builds, whitespace formatting and diff
checks pass. Spec/API docs, CLI bridge notes and changelog describe the shared
contract and corrected metadata. Native guest execution and Framework/NanoFramework
are not claimed; the neoCLR check uses host references. No syntax/highlighting
change or RavenDoc/site generation is required.

TypeMemberBinder is not yet translated. This C# bootstrap correction benefits
the unchanged Raven generated abstract declarations when built with the updated
compiler. The full emission in session 39453 still uses immutable PORT-101 and
therefore predates PORT-103/104. Its result remains useful for exposing emission
gaps, but final qualification must use the updated compiler. Source hashes remain
unchanged; do not replace the active process. Its latest verified PID is 65774.

## Raven syntax API completion checkpoint

Author direction (2026-11-02): complete the syntax API, format the Raven code with
opening braces on the same line, then commit the syntax port. Keep C# as the
bootstrap implementation and retain shared C# tests. The pending syntax source
batch is not yet qualified for that commit.

### Required surface

| Area | Current evidence and remaining work |
| --- | --- |
| Generated red/green nodes, factories, visitors, rewriters | The refreshed 1,095-source snapshot emitted successfully and passed all 339 Raven consumer checks; the paired C# consumer passes 334. This validates the frozen syntax-runtime snapshot, including the generic selector fix. Newly translated cyclic grammar parsers remain separately validated until their dependency cycle closes. |
| Handwritten tokens, trivia, lists, nodes, locations and diagnostics | Pending Raven translations exist. Validate behavior and assembly ownership, not just successful source binding against bootstrap references. |
| Small handwritten node/factory conveniences | Added ArrayExpressionSyntax, AssignmentStatementSyntax, CollectionExpressionSyntax, IdentifierName(string), and Block() translations. Syntax parsing passes; they are included in the current full emission snapshot. |
| Macro and attribute syntax helpers | Translations now exist with same-line opening braces. The extension helper emits and matches eight C#/Raven runtime comparisons using bootstrap-owned nodes; full source-owned parity remains pending. |
| Node extension/reflection helpers | SyntaxExtensions and SyntaxNodeReflectionExtensions are translated; reflection list membership was corrected in both versions. Reduced reflection-helper comparisons pass using bootstrap-owned nodes; the final context-fix rerun passes seven comparisons; full source ownership remains pending. SyntaxNodeExtensions and NormalizeWhitespace are now translated; reduced shared tests pass using bootstrap-owned nodes. Final full-source validation remains pending. |
| Parsing entry points | Raven parser contexts/checkpoints/common recovery helpers pass 20 foundation checks. Seventeen declaration/name/directive/pattern/statement parser classes, including type declarations, compilation-unit, namespace, extension, enum, union and macros, now execute together in a Raven component: 301 shared C# and ownership checks pass. The expression parser and its interpolation partial plus LanguageParser remain to be ported and integrated. Green/red runtime nodes and contexts remain bootstrap-owned in the declaration component. Public SyntaxFactory parsing methods and SyntaxTree.ParseText still require the complete grammar cycle; do not substitute bootstrap delegation. |
| Incremental text and submissions | Port SyntaxTree.Parsing, SyntaxTree.Submissions and required parser/incremental dependencies; validate WithChangedText and submission completeness. |
| Generated-code handling | Translated; reduced emission succeeds after ref-to-in applicability fix. The parenthesized-pattern correction restores 16 classifier comparisons with concurrent reads; final full-source ownership validation remains pending. |
| Formatting and diagnostics presentation | Formatter/SyntaxNormalizer translated; all 13 shared formatting tests pass against the Raven component after the generic closure field fix. PrettyGreenTreePrinter translated; 8 printer/annotation tests and an ownership check pass against the Raven component. The iterator receiver defect is fixed; all 11 printer/node component checks now pass. Validate the fully source-owned runtime. |
| Small remaining bootstrap dependencies | DocumentationFormat is now translated (final integration validation pending); symbol contracts such as IModuleSymbol remain bootstrap dependencies. Record ownership explicitly; do not mistake a driver-added compiler reference for Raven implementation coverage. |

### Layout and generators

Use opening braces on the same line and blank lines between declarations in
handwritten Raven syntax code and in
Raven output from both C# and Raven generator implementations. Preserve comments,
string/token contents, accessibility and semantics. Update emitter templates rather
than only reformatting a disposable generated directory. Run the matching generator
checks for both implementations and preserve C# output. Do not generate RavenDoc or
the website during the port.

Before formatting edits, the existing Raven-output generator tests pass 21 cases
against each generator implementation. Logs are
/tmp/syntax-format-generator-csharp-baseline.log and
/tmp/syntax-format-generator-raven-baseline.log.

### Active validation and next gate

The previous immutable snapshot completed successfully: **1,095 sources**,
1,840 warnings, 1741.1 seconds, exit 0 (`/tmp/syntax-api-stabilized-emit.log`).
Its assembly `/tmp/raven-syntax-runtime-stabilized/Raven.CodeAnalysis.Raven.dll`
passed **327 C# consumer checks** (`/tmp/syntax-api-stabilized-consumer.log`),
including owned syntax/runtime types and loading all assembly types. This is
coverage of the implemented surface, not completion of missing parser APIs.

Completed full emission: **1,095 sources**, session **72506** (exit 0), immutable snapshot
`/tmp/raven-syntax-current-model-snapshot`, seed
`/tmp/raven-syntax-current-model-seed`, log `/tmp/syntax-api-current-model-emit.log`,
output `/tmp/raven-syntax-runtime-current-model/Raven.CodeAnalysis.Raven.dll`.
It incorporates current generated attribute slot ordering and token missing-state
preservation. The newer cyclic grammar parser components remain excluded until
their expression/statement/type-declaration dependencies are source-owned.

Paired consumers are prepared at `/tmp/syntax-runtime-current-model/CSharp` and
`/tmp/syntax-runtime-current-model/Raven`, with new printer, node annotation and
missing-token regressions. Cross-assembly generated catalog/kind/facts checks
remain Raven-only checks, comparing the port to the seed. The Raven consumer has now run: 338 passed and one failed; see the final checkpoint below. Do not infer current-source parity from the earlier
frozen snapshot or bootstrap-owned dependencies in reduced parser tests.

Final acceptance requires the completed API, an updated full build, both
generator outputs, Raven formatting and shared C# tests against the final Raven
assembly. The broader pattern suite has one independently verified pre-existing
generic hierarchy inference failure. Detailed chronology, reductions, logs and
validation scope are recorded in `raven-port-progress.md`.

#### Union declaration parser and inherited property patterns

- Translated `UnionDeclarationParser` directly to Raven, retaining nominal cases,
  structural member lists, modifiers, constraints, separators and recovery. The
  parser remains inheritable, with private implementation helpers.
- The translation exposed shared binder diagnostic RAV1602 for inherited members
  in property patterns (`TerminatorToken.Kind` and `IsMissing`). Pattern lookup
  now uses `SymbolQuery` and ordinary lazy source-signature completion. The binder
  is still C#-owned; this shared correction applies before target emission.
- Focused baseline: 27 property-pattern tests passed. Five new source/metadata
  runtime cases failed before the fix and pass afterward, including nested paths
  and both matching and nonmatching values. Final combined C# run: 59 passed
  (`/tmp/union-inherited-pattern-final.log`).
- Raven component emitted successfully with one existing nullable-suppression
  warning (`/tmp/union-parser-fixed-emit.log`). Shared C# union contracts plus
  Raven ownership check: 12 passed (`/tmp/union-parser-raven-parity.log`).
  These tests execute the Raven parser with bootstrap-owned grammar dependencies;
  they do not establish a fully Raven-owned parser or native neoCLR execution.
- Full current-model emission session 72506 remains live at this checkpoint;
  its frozen inputs do not include this newer union parser component.

#### Namespace parser: enum ordering, recovery and sibling closures

- Translated `NamespaceDeclarationParser` directly, including braced/file-scoped
  declarations, `module` tokens, modifiers, imports/aliases, nested declarations,
  declaration separators and recovery. Helpers remain private; the class remains
  inheritable. Mutable C# parameters use local mutable copies in Raven.
- Baselines: 278 namespace/declaration tests and 162 enum tests passed. The port
  exposed missing same-enum relational operators. Shared binding now admits them;
  CLI comparisons honor the underlying signedness. Portable lowering carries the
  same behavior for its existing Int32 enum capability. Four signed/unsigned
  .NET runtime cases and four portable admission cases pass.
- Misplaced imports/aliases previously reordered source during recovery. Both
  C# parser entrypoints now retain them as skipped syntax at their original
  positions. Raven namespace parsing and the common parser helper mirror this.
  The affected union diagnostic fixture now places imports before declarations.
- The namespace component exposed incompatible closure frames for self-only
  local functions called by siblings. CLI closure setup now resolves their
  lexical method owner before defining hidden parameters; source local-function
  symbols themselves can be stored under the containing type. The namespace
  runtime crash no longer reproduces. No Raven workaround replaces the local
  functions, and native VM execution is not claimed.
- Current focused C# validation: 77 passed, including adjacent closure/iterator
  coverage (`/tmp/namespace-closure-final-tests.log`). Raven namespace component:
  12 passed (`/tmp/namespace-parser-lexical-parity.log`), emitted with one existing
  checkpoint-disposal warning. Grammar dependencies remain bootstrap-owned.
- An additional gap surfaced while constructing enum tests: metadata constant
  fields such as `uint.MaxValue` / `ulong.MaxValue` are rejected as enum member
  initializers with RAV0412. Keep this open for the next stabilization slice.
  The signedness runtime tests pass actual high unsigned enum values through
  reflection so they test comparisons independently of constant evaluation.
- Full syntax snapshot session 72506 remains live and predates this component.
  Its completion will not establish current namespace/foundation source parity.

Final namespace slice validation: **465 C# tests passed** across enum, namespace,
directive-order and declaration-separator coverage
(`/tmp/namespace-enum-verified.log`). The revised Raven parser foundations,
including source-owned `PreserveMisplacedDirective`, passed **20 shared C# and
ownership checks** (`/tmp/syntax-parser-recovery-final-parity.log`). Both focused
and broader checks now pass; no native neoCLR execution was performed. Touched
C# files were whitespace-formatted, Raven uses same-line opening braces and
blank declaration spacing, and `git diff --check` passes.


#### Full current-model snapshot completion

Session 72506 completed successfully: 1,095 sources, 1,840 warnings, 1485.1 seconds,
exit 0. The expanded Raven consumer suite then reported **338 passed, 1 failed**
(`/tmp/syntax-api-current-model-raven-consumer.log`). The failing shared case is
`PrettyGreenTreePrinterContractTests.FlattenedListsRetainOriginalSlotIndices`:
`SyntaxFactory.List<TNode>(IEnumerable<TNode>)` throws `BadImageFormatException`
in its generated selector lambda (`SyntaxList`1.rvn`, line 108). The matching
C# consumer previously passed 334 tests (the Raven suite additionally includes
five cross-assembly identity/catalog checks). Preserve this failure and reduce
its generic method/lambda metadata before refreshing the next full snapshot.
The copied-token missing-state and annotation regressions pass in the owned
Raven assembly. This result supersedes earlier notes that session 72506 was live.
The full syntax API and final syntax-batch commit remain incomplete.

#### Generic selector metadata and enum initializer stabilization

- Reduced the full-assembly `SyntaxFactory.List<T>` failure to a C# runtime test
  compiling a generic LINQ selector in Raven. It reproduced the same
  `BadImageFormatException`. Captureless lambdas now retain enclosing generic
  method context through its closure frame. A second regression with a generic
  enclosing type exposed alias-slot confusion; method aliases now select the
  trailing closure parameters, keeping type and method parameters distinct.
- Four new selector cases plus existing generic/sibling/iterator closure coverage
  pass: **14 tests** (`/tmp/generic-selector-alias-tests.log`). The Raven syntax
  source remains a direct translation; no selector workaround was introduced.
- The prior enum-constant gap is resolved. Enum initializers now use the shared
  binder constant evaluator, including metadata fields and arithmetic. Eight
  initial regressions reproduced failures; the final suite also covers `1 + 1`.
  Removed the stale expectation that `1 + 1` is nonconstant and retained a real
  nonconstant case (`System.Environment.TickCount`). Maximum-valued members are
  valid; implicit overflow is diagnosed on the following member that needs it.
- Enum semantic/runtime/portable adjacent coverage: **39 passed**
  (`/tmp/enum-constant-verified.log`). This is modern .NET and shared portable
  contract validation, not native neoCLR execution. Compiler semantic/codegen
  layers are still C#-owned; corresponding Raven layers have not yet been ported.
- A fresh immutable **1,095-source** build is running as session **90333**:
  `/tmp/raven-syntax-generic-selector-snapshot`, seed
  `/tmp/raven-syntax-generic-selector-seed`, log
  `/tmp/syntax-api-generic-selector-emit.log`, output
  `/tmp/raven-syntax-runtime-generic-selector/Raven.CodeAnalysis.Raven.dll`.
  It includes the generic selector compiler fix and current syntax foundations;
  its compiler seed predates the later enum-constant correction. Do not mutate
  that seed or restart the live process. Paired consumers are prepared under
  `/tmp/syntax-runtime-generic-selector/{CSharp,Raven}`. Run the Raven consumer
  only after successful emission. New cyclic grammar parsers remain separately
  validated and excluded until their source-owned dependencies are available.

The refreshed seed's C# syntax consumer passes **334 checks**
(`/tmp/syntax-api-generic-selector-csharp.log`). Session 90333 was confirmed live
at this checkpoint. Whitespace formatting and `git diff --check` pass; syntax
completion and its final batch commit are still pending.

#### Compilation-unit parser and combined declaration parser validation

- Translated `CompilationUnitSyntaxParser` directly to Raven, including compilation
  attributes, directives, global import blocks, declarations, top-level statements,
  modifier handling and the single-member entry point. The class remains
  inheritable with private helpers. Removed the same unused lookahead local in
  C# and Raven. Raven formatting uses same-line braces and declaration spacing.
- Baseline: 51 focused compilation-unit/directive/entry-point checks passed.
  Added shared source/shape/EOF contracts for empty input, trivia, namespaces,
  module tokens, imports/aliases, assembly attributes, global imports, top-level
  statements, functions and macros.
- The preceding misplaced-directive recovery changed the internal single-member
  entry point to return an incomplete member for an import/alias. Two tests
  reproduced this in both implementations. Start fragment parsing in the imports
  state so standalone directives retain the prior null result.
- Four new malformed global-import cases reproduced silent token loss. Both
  implementations now preserve unexpected tokens as skipped trivia on the next
  import keyword or closing brace, retaining text even when the brace is missing,
  and report the relevant diagnostics. No new syntax model or grammar is required.
- C# validation: **160 passed** (`/tmp/declaration-parser-cluster-recovery-csharp.log`).
  The combined Raven declaration parser assembly builds with 18 warnings
  (`/tmp/declaration-parser-cluster-emit.log`). Its inputs are 13 completed parser
  classes, the Raven attribute extension, and test friend access; see
  `/tmp/declaration-parser-cluster/command.json`. It uses the current seed at
  `/tmp/raven-compilation-parser-seed`.
- Unlike earlier isolated checks, compilation-unit calls now use Raven-owned
  name, directive, constraint, attribute, enum, union, namespace and macro parser
  implementations together. Expression, statement, type and extension declaration
  parsers, green/red runtime nodes and parser contexts are still bootstrap-owned
  in this component. This does not complete the source-owned grammar cycle.
- Full syntax rebuild session 90333 remains live on its frozen earlier inputs;
  it does not include this new compilation-unit parser.


Final combined declaration-parser validation: **146 shared C# and ownership checks
passed** against the Raven assembly
(`/tmp/declaration-parser-cluster-verified-raven.log`); the matching C# contracts
plus adjacent public entry-point/global-import tests passed **160 checks**.
Touched C# files were whitespace-formatted; `git diff --check` passes. This
completes the compilation-unit component slice, not the full syntax API or its
pending commit. Extension/type/statement/pattern/expression parser dependencies
remain to be translated.


#### Extension declaration parser and recovery

- Translated `ExtensionDeclarationParser` directly, preserving its closed class
  shape, private helpers, generic constraints, methods, properties, indexers,
  events, operators, conversions and accessor declarations. The Raven source
  uses same-line opening braces and blank lines between declarations.
- Baseline: 16 adjacent extension semantic/recovery tests passed. New shared
  contracts exposed source-token loss in malformed members and accessors in the
  C# implementation. Both sources now retain those tokens as skipped syntax and
  preserve diagnostics. No grammar, highlighting or runtime-specific change is
  required.
- Validation on modern .NET: 32 focused C# tests passed
  (`/tmp/extension-parser-recovery-tests.log`). The combined Raven component now
  includes 14 parser classes and passes 162 shared contract and ownership checks
  (`/tmp/extension-parser-cluster-raven.log`). Emission succeeded with 19 warnings.
  Compilation-unit calls now also use the Raven extension declaration parser.
- Syntax nodes, contexts and remaining expression/statement/type dependencies
  are still bootstrap-owned in this component. These checks do not establish
  native neoCLR execution or completion of the source-owned syntax API.
- Touched C# sources and tests were whitespace-formatted. The full syntax build
  remains active on its frozen earlier input; final syntax qualification and the
  requested syntax batch commit remain pending.


#### Completed generic-selector syntax-runtime validation

The frozen 1,095-source snapshot completed with exit 0 in 1,428.6 seconds
(1,840 warnings). All **339 Raven consumer checks pass** in
`/tmp/syntax-api-generic-selector-raven-consumer.log`; its paired C# consumer
passed 334. This supersedes the earlier 338/339 result and verifies the generic
selector correction against the actual Raven syntax runtime. Build evidence is
`/tmp/syntax-api-generic-selector-emit.log`; the immutable inputs and command are
under `/tmp/raven-syntax-generic-selector-snapshot`.

The newer declaration parser components are not included in this snapshot.
Public parsing entry points, the remaining grammar dependencies and final syntax
batch qualification remain incomplete. Validation ran on modern .NET, not native
neoCLR.


### Pattern parser translation and shared stabilization

- Translated `PatternSyntaxParser` beside C#, preserving its inheritable class,
  private options/helpers, logical/comparison/range patterns, property/sequence/
  dictionary/positional patterns, designations and speculative checkpoints.
  Indexed loops use equivalent Raven while loops; C# switches become matches.
  Opening braces and declaration spacing follow the Raven convention.
- Baseline: 47 existing pattern syntax tests passed. Shared internal entry-point
  contracts exercise source/shape/EOF behavior, implicit bindings, precedence,
  guards and incomplete-input recovery. They execute the Raven parser directly
  in the component assembly rather than going through bootstrap public factories.
- Comparison/range operands and identifier constants previously consumed outer
  `and`, `or` and `|` tokens as expression operators. Both pattern sources now
  request an explicit expression-parser boundary. Parenthesized operands keep
  ordinary operators. Public syntax regressions cover the same cases.
- Compiling the direct translation exposed false `RAV0269` diagnostics for an
  `out` parameter assigned in an `if` condition. The shared binder now analyzes
  that condition before either branch and follows unary operands. Four positive
  repros failed before the correction; short-circuit negative coverage remains.
  The owning binder has not yet been ported, so there is no Raven binder copy to
  patch at this stage.
- Recovery contracts exposed missing closing-delimiter diagnostics. Both parser
  implementations now use a local required-token helper that reports
  `CharacterExpected` while preserving missing tokens and source text. Property
  clause speculation still rewinds diagnostics when rejecting a clause.
- A translation-only missing loop increment caused the property-pattern case to
  hang. Sequential execution isolated it; the increment is restored and the
  existing property contracts pass. No Raven language workaround was introduced.
- Final modern .NET validation: **171 C# tests pass**
  (`/tmp/pattern-parser-recovery-csharp.log`), including pattern/range and by-ref
  semantic/runtime adjacency. The 15-parser Raven component emitted successfully
  with 30 warnings and passes **207 shared contract and ownership checks**
  (`/tmp/pattern-parser-verified-raven.log`). Inputs are recorded in
  `/tmp/declaration-parser-cluster/command.json`. Touched C# files and tests were
  whitespace-formatted; `git diff --check` passes.
- The component still uses bootstrap expression/statement/type parsers, contexts
  and green/red nodes. Its own pattern calls now use Raven code. No native neoCLR
  execution is claimed; these parser and binder rules are shared before target
  lowering. No highlighting token or runtime bridge encoding changed. Language
  docs/grammar, compiler API docs, changelog and target notes record the fixes;
  website generation and ravendoc generation were not run.
- The 1,095-source syntax-runtime snapshot has separately passed 339 checks; it
  predates this grammar slice. Public parsing entry points, the remaining grammar
  cycle and the final syntax batch commit are still pending. The next dependency
  slice is the statement/type/expression grammar cycle.

The earlier committed port history above is retained in full. The syntax
checkpoint had temporarily replaced it in the working tree; restoring it keeps
prior stabilization records alongside the current evidence.


### Statement parser translation and local-function reachability

- Translated `StatementSyntaxParser` directly, preserving the inheritable class,
  private helpers and abrupt-transfer parsing mode. The source covers control
  flow, labels, local functions/types, local declarations, parameter patterns,
  exception handling, scoped resource declarations, macros and recovery. Nested
  statement parsers inherit the expression-mode flag as in C#. Raven formatting
  uses same-line braces and blank lines between declarations.
- Removed an unused incomplete-statement lookahead and explicitly initialized
  the nullable `LastStatement` property to null in both sources. Corrected the
  Raven pattern parser's diagnostic interpolation spelling to preserve its C#
  message. There is no syntax-model/generator change in this slice.
- Baseline: 79 statement/parameter tests passed. Added 51 shared internal-parser
  contracts for source and node shape, EOF, parameter lists, skipped tokens,
  abrupt-transfer expression mode and inheritance of that mode in nested blocks.
- Emission exposed false unreachable-code warnings on local functions declared
  after terminal code. Four reduced public `AnalyzeControlFlow` cases reproduced
  the defect before correction. The shared control-flow walker now excludes
  local declarations from enclosing executable flow without reviving the endpoint.
  A separate regression confirms that unreachable statements inside a local
  function still receive diagnostics. The semantic-model implementation has not
  yet been ported, so there is no Raven copy of that walker to patch yet.
- Final modern .NET validation: **181 C# checks pass**
  (`/tmp/statement-parser-verified-csharp.log`), including statement/parameter and
  control-flow/throw-diagnostic adjacency. The 16-parser Raven component builds
  with 46 warnings in 102 seconds and passes **258 shared contract and ownership
  checks** (`/tmp/statement-parser-verified-raven.log`). The declaration assembly
  and recorded command remain under `/tmp/declaration-parser-cluster`.
- Compilation-unit, namespace, extension and macro calls now use Raven-owned
  statement/parameter parsing where the translated source calls it. Expression
  and type parsers, green/red nodes and contexts are still bootstrap-owned in this
  component. Calls made inside those bootstrap parser dependencies remain C#.
  These tests do not establish native neoCLR execution or close the grammar cycle.
- Compiler API docs, local-function language docs, changelog and target integration
  notes record the shared reachability correction. No grammar token, highlighting
  rule or CLI bridge encoding changed. C# formatting and `git diff --check` pass;
  ravendoc and website generation were not run.
- Next dependency slice: `TypeDeclarationParser`, followed by the expression parser
  and interpolation partial, `LanguageParser`, and integration of source-owned
  public parsing entry points. Final syntax API qualification and its requested
  batch commit remain pending.


### Type-declaration parser and explicit indexer stabilization

- Translated `TypeDeclarationParser` beside C#, preserving the inheritable class,
  private helpers, class/struct/record/interface headers, base/permits lists,
  constructors (including `self` and `base` initializers), members, accessors,
  operators, delegates, case fields and parameter parsing. Raven code uses
  same-line opening braces and declaration spacing, with direct control flow.
- Baseline: 49 focused class/struct/property/parameter checks passed. Added 43
  shared contracts for whole declarations, member shapes, explicit interfaces,
  recovery and lookahead checkpoint restoration. The initial C# run failed two
  contracts; the initial Raven run reproduced exactly those two failures, with
  its other 299 checks passing.
- Both sources now recognize reserved `self` after an interface qualifier when
  parsing an explicit indexer. `self` is not made an ordinary type identifier.
  Both accessor-list parsers preserve unexpected input in skipped trivia on a
  missing accessor keyword instead of discarding it.
- End-to-end .NET validation exposed missing explicit-interface symbol links.
  The shared binder now records the source indexer's interface property and the
  interface getter for expression-bodied indexers. Accessor-list methods retain
  their existing implementation links. New symbol/runtime cases verify getter,
  setter and expression-bodied getter dispatch through the interface, returning
  the expected value. The binder itself is not ported yet, so there is no Raven
  binder source copy to change at this stage.
- Final validation: **156 C# checks pass**
  (`/tmp/type-parser-verified-csharp.log`), covering class/record/struct/property
  syntax, parameter lists, interface accessibility, generic implementation flags,
  cross-tree explicit implementations and runtime dispatch. The 17-parser Raven
  component builds with 55 warnings in 125.1 seconds and passes **301 shared
  contract and ownership checks** (`/tmp/type-parser-verified-raven.log`).
- Translated calls now use the Raven type parser and its statement/pattern/name
  dependencies together. Expression parsing and its internally constructed
  parsers remain bootstrap-owned, as do contexts and syntax nodes in this reduced
  component. The separate full syntax-runtime snapshot remains the previously
  qualified 1,095-source/339-check snapshot; these new grammar parsers are not
  included in it yet.
- Changelog, syntax/semantic API docs, interface language docs, grammar and target
  integration notes record the fixes. No highlighting token or new bridge
  representation was added. Runtime evidence is modern .NET only; shared parser
  and binder corrections apply before target-specific lowering. C# whitespace
  formatting and `git diff --check` pass; no ravendoc or website generation ran.
- Next: port `ExpressionSyntaxParser` and its string interpolation partial, then
  `LanguageParser`; close the source-owned grammar cycle and validate public
  `SyntaxFactory`/`SyntaxTree` parsing APIs before the pending syntax batch commit.

### Interpolation helper port and stabilization (2026-11-02)

- Extracted the C# interpolation partial into internal static
  `StringInterpolationParser`, retaining the scanning algorithms and existing
  expression-parser call sites. Added its direct Raven translation. Embedded
  expressions still depend on bootstrap `LanguageParser`; the full expression
  parser and public parsing entry points are not yet ported.
- Added 32 shared C# contracts for detection, escaping, interpolation boundaries,
  missing end tokens, raw multiline text, inner text and outer trivia. Replaced
  an obsolete private-reflection trivia test with direct helper access and
  current Raven interpolation syntax.
- The initial translation exposed a parenthesized-header/cast ambiguity.
  `if (ready)` followed by a line-broken assignment incorrectly consumed the
  body as a cast operand. Shared expression-parser lookahead now preserves the
  header boundary. Three added statement contracts cover if, while and a real
  same-line cast. Focused C# validation: 216 passed
  (`/tmp/interpolation-final-csharp.log`); adjacent conditional/loop/cast parser
  validation: 39 passed (`/tmp/interpolation-header-adjacent.log`).
- Initial Raven execution passed 331/332 checks. Raw multiline text was wrongly
  decoded because the compiler equated the two same-signature local
  `AddTextSegment` functions in different methods. A reduced test failed for
  both capturing and noncapturing functions before the fix. Shared symbol
  equality and hashing now include local-function declaration identity.
  The reduced regressions plus symbol/closure tests pass 36/36
  (`/tmp/interpolation-identity-tests.log`).
- Final Raven parser component build succeeded in 139.4 seconds with 60 warnings
  (`/tmp/declaration-parser-cluster-emit.log`). All 336 shared C# and ownership
  checks pass (`/tmp/interpolation-final-raven.log`), including the previously
  failing raw-text check. The component contains 17 parser classes plus the
  source-owned interpolation helper; bootstrap syntax runtime/contexts and
  embedded-expression parsing remain explicit dependencies.
- Source and test whitespace formatting and `git diff --check` pass. Changelog,
  language rules, compiler API docs and neoCLR integration notes record both
  fixes. No ravendoc or website generation ran. These are modern .NET results,
  not native neoCLR execution or completed public syntax API integration.
- Next: port the main `ExpressionSyntaxParser`, then `LanguageParser`, close
  the grammar cycle and qualify the complete public syntax API before the
  pending syntax batch commit.

### Main expression parser translation (2026-11-02)

- Targeted baseline: 1,762 syntax checks passed before this slice
  (`/tmp/expression-port-syntax-baseline.log`).
- Translated the complete C# `ExpressionSyntaxParser` into Raven, including
  operators, assignments, lambda lookahead/bodies/attributes, calls and trailers,
  collections, macros, matches, conditionals and recovery. The translation keeps
  the algorithms and explicit private members. Per author direction, unconditional
  `while (true)` loops become Raven `loop` statements. Indexed loops preserve
  their increments on both ordinary and continue paths.
- Added 44 shared expression contracts. The first C# run exposed missing
  closing-delimiter diagnostics in tuples and collections; both parser sources
  now report required closing delimiters while retaining missing tokens and input.
  C# syntax validation passed 1,806 checks
  (`/tmp/expression-delimiter-csharp.log`).
- Compiling the translation exposed missing definite assignment for forwarded
  `out` calls in a `while` condition. Four focused positive cases failed before
  the shared method-body analyzer fix; short-circuit negative cases remained
  diagnostic. After the fix, 107 by-ref/assignment checks pass
  (`/tmp/expression-while-out-fixed.log`).
- The full Raven parser component built successfully in 211.7 seconds with
  116 warnings (`/tmp/declaration-parser-cluster-emit.log`). All 406 shared C#
  and ownership checks pass (`/tmp/expression-component-tests.log`), including
  the 44 new expression contracts and existing C# return/throw/await parser tests.
  The tests select the Raven expression-parser type explicitly; the component
  now contains 18 parser classes plus the interpolation helper.
- C# whitespace formatting and `git diff --check` pass. The Raven translation
  uses same-line opening braces and blank lines between declarations. Changelog,
  syntax/semantic API docs and target integration notes record the two shared
  fixes. Existing grammar and highlighting tokens remain unchanged.
- `LanguageParser` and its `ParseResult` remain the next dependency; bootstrap
  syntax runtime/contexts and embedded interpolation parsing still prevent a
  claim of fully source-owned public parsing. No syntax batch commit or ravendoc/
  website generation has run.


### Language parser and immutable local declarations (2026-11-02)

- Ported `LanguageParser` and the value-record `ParseResult`, closing the grammar
  parser cycle within the component. Added 16 shared entry-point/result contracts
  and reused parser-context contracts. Specialized name requests previously fell
  through to general expression parsing; the initial C# baseline exposed this
  failure (17 passed, one failed). Both implementations now dispatch names first.
- Applied the author-directed `let` convention to parser locals that are never
  reassigned, including single-assignment `out let` declarations. Retained mutable
  locals and mutable properties. Recorded this convention and the permitted
  `while (true)` to `loop` translation in AGENTS and the bootstrap procedure.
- This cleanup exposed shared binder defects: declaring calls incorrectly rejected
  readonly output initialization, untyped output declarations could prevent overload
  selection, and generic pre-inference treated newly declared outputs as reads.
  The shared binder now distinguishes initialization from subsequent writes,
  obtains output types from the selected parameter, and excludes unknown outputs
  from overload preference and generic input inference. Later readonly writes
  remain errors; overloads differing only in unknown output type remain ambiguous.
  Declaration-state/cache entries are refreshed with the resolved local type.
- The required generator/solution build succeeded (`/tmp/out-declaration-model-build.log`).
  Final focused C# validation passed 158 by-ref, assignment, overload and parser
  checks (`/tmp/out-declaration-final-tests.log`). A separate runtime regression
  validates the documented `Math.DivRem(..., out let remainder)` initializer and
  following use (one passed, `/tmp/out-declaration-doc-example.log`).
- The final Raven parser component built in 208.2 seconds with 117 warnings and
  passed all 424 shared C# and ownership checks
  (`/tmp/language-parser-raven-final.log`). It contains 19 parser classes, the
  interpolation helper and ParseResult. Lexer, contexts and green/red syntax
  runtime remain bootstrap-owned dependencies of this component.
- Changelog, language reference, .NET interop website source, semantic/syntax API
  docs and neoCLR bridge notes record these corrections. Validation executes on
  modern .NET only; native neoCLR execution remains unverified. No RavenDoc or
  website generation ran.
- Public syntax API completion remains pending. In particular, integrate Raven
  SyntaxFactory parsing and SyntaxTree parsing/incremental/submission APIs with
  the source-owned runtime. Review the existing C# entry points that discard
  parse diagnostics or ignore `consumeFullText` in statement parsing, and detached
  node diagnostic retrieval, with shared regressions before claiming parity.
  The pending syntax batch remains uncommitted until that completion gate passes.


### Internal parsing entry-point diagnostics (2026-11-03)

- The preceding immutable-local slice made verified progress: 159 focused C#
  checks and 424 Raven component checks passed. This slice advances the pending
  public parsing API integration by correcting its internal entry points first.
- Existing LanguageParser contracts passed before changes. Three additional
  cases exposed two failures: node-only ParseSyntax dropped recovery diagnostics,
  and ParseStatement ignored consumeFullText. C# and Raven now preserve context
  diagnostics on node-only green roots and use the result-producing statement
  path for full-input validation. The ParseResult API still returns its diagnostic
  list separately, avoiding duplicate attachment by its callers.
- Targeted C# build and 117 parser checks pass (`/tmp/parser-entry-tests.log`).
  Source/test whitespace formatting and diff checks pass. Changelog and syntax
  API documentation record the behavior and explicitly retain the outstanding
  public detached-node diagnostic retrieval/integration work.
- Final Raven component build succeeded in 197.0 seconds with 118 warnings;
  all 427 shared C# and ownership checks pass (`/tmp/parser-entry-raven-tests.log`).
  The reduced component still uses bootstrap lexer, contexts and syntax runtime.
  Validation ran on modern .NET; no native neoCLR execution or documentation
  generation was performed. Public API completion and the syntax batch commit
  remain pending.


### Public factory parsing translation (2026-11-03)

- The previous internal-entry-point slice made verified progress (117 focused C#
  and 427 Raven component checks passed). This slice translates all public
  SyntaxFactory parsing overloads into a separate partial file, preserving the
  bootstrap API, fragment offsets, fallback shapes and member restrictions.
  Locals use let and Raven declaration formatting is retained.
- Added 11 shared C# factory contracts covering string/SourceText overloads,
  offsets, source preservation and member acceptance/rejection. These plus the
  internal entry-point contracts pass 30/30 against C#
  (`/tmp/factory-parser-csharp.log`). Public detached-node diagnostics and
  compilation-unit factory diagnostic retention remain explicit pending gaps;
  this translation does not redefine their current behavior as final correctness.
- The Raven parser/factory component built successfully in 215.8 seconds with
  118 warnings. All 439 shared C# and ownership checks pass
  (`/tmp/factory-parser-raven.log`), including the 11 new factory contracts and
  explicit assembly ownership of the public factory and LanguageParser.
  This validates source-owned factory/grammar code with bootstrap-owned lexer,
  contexts and green/red syntax runtime, not full runtime integration.
- Test whitespace formatting and git diff checks pass. No RavenDoc/website
  generation ran; validation executes on modern .NET, not native neoCLR.
  Next: SyntaxTree parsing, incremental/submission APIs and diagnostic retrieval,
  then full-runtime integration and the pending syntax batch commit.


### SyntaxTree parsing and submission translation (2026-11-03)

- The preceding factory slice made verified progress: 30 focused C# checks and
  439 Raven component checks passed. Before this slice, 30 tree/submission checks
  and 39 incremental-update checks passed against C#
  (`/tmp/syntax-tree-port-baseline.log`, `/tmp/syntax-tree-incremental-baseline.log`).
- Translated SyntaxTree.Parsing, including full parsing, generated-text parsing,
  incremental replacement, recovery/fallback policies, diagnostic span updates,
  fallback reason/exception and SyntaxFactory.ParseSyntaxTree overloads.
  The candidate-parent loop preserves advancement on every continue path.
  Added SyntaxTree.Submissions and the SubmissionCompleteness enum, preserving
  lexical/directive/trailing-missing-syntax classification and cancellation.
- Local values use let unless reassigned. ParseTextCore attaches the final root
  directly instead of reassigning a temporary. All three new files parse without
  diagnostics (`/tmp/syntax-tree-port-parse-check.log`); this is syntax validation,
  not evidence that binding or emitted runtime behavior is correct.
- Started full integration build in session 47716: immutable 1,116-source inputs
  `/tmp/raven-syntax-public-integration-snapshot`, manifest with hashes and exact
  command; seed `/tmp/raven-syntax-public-integration-seed`; output expected at
  `/tmp/raven-syntax-runtime-public-integration/Raven.CodeAnalysis.Raven.dll`;
  log `/tmp/syntax-api-public-integration-emit.log`. It includes source-owned
  runtime, lexer, contexts, grammar, factories and tree APIs. Poll the same
  process/handle until terminal; do not restart from an observation timeout.
- Expanded paired consumers at `/tmp/syntax-runtime-public-integration` reuse
  factory, tree, submission and incremental C# tests. The C# consumer passes
  414 checks (`/tmp/syntax-public-integration-csharp.log`). The Raven consumer
  has not run: full emission is pending, and incremental tests also require
  the unported public PrettySyntaxTreePrinter/PrinterOptions API. Preserve those
  tests and port that API rather than mistaking bootstrap printing for coverage.
- Public detached-node diagnostics and compilation-unit factory diagnostic
  retention remain known gaps. Full integration acceptance, permanent project
  wiring and the syntax batch commit remain pending. No RavenDoc/website
  generation ran; no native neoCLR execution is claimed.


### Public syntax-tree printer translation (2026-11-03)

- The previous tree API slice made progress by translating parsing/incremental/
  submission APIs and starting the immutable integration build. Re-polled session
  47716 and confirmed its compiler process remains active; no restart occurred.
- Translated PrinterOptions and the complete PrettySyntaxTreePrinter into Raven,
  including extension entry points, list grouping, trivia/structured trivia,
  locations/spans, colors and annotation/diagnostic presentation. The private
  PrintableChild helper retains its class/reference semantics and constructors.
  Locals use let unless mutated. Nullable annotations follow the reflected APIs;
  GetTokenText no longer takes an unnecessary ref parameter in either source.
- Existing printer baseline passed two checks. Three new finite-depth cases
  exposed two failures: traversal began at MaxDepth rather than zero. A further
  trivia-depth case failed because trivia ignored the bound. Both implementations
  now count from zero and bound trivia/structured-trivia and child presentation.
  The seven printer cases plus 39 adjacent incremental cases pass against C#
  (`/tmp/tree-printer-final-csharp.log`).
- The Raven printer component emits successfully (18.4 seconds, 29 warnings;
  `/tmp/tree-printer-final-emit.log`). Final component validation passes 16 checks
  (`/tmp/public-printer-final-parity.log`): seven shared cases, explicit assembly
  ownership, and eight exact-output comparisons covering 32 presentation
  combinations. Nodes/parser remain bootstrap-owned in this reduced component.
- Initial component aliases used the same names as enclosing namespace types,
  allowing the shared tests to select bootstrap code. Unique TestPrinter and
  TestPrinterOptions aliases plus an explicit ownership assertion corrected the
  harness. Only the final 16-check run is authoritative Raven printer evidence;
  it supersedes the earlier five-check result.
- Changelog/API docs record the depth fixes; whitespace formatting and diff
  checks pass. No RavenDoc/website generation or native neoCLR execution ran.
- The ongoing 1,116-source snapshot predates this printer translation and the
  latest C# printer corrections. Keep it immutable and inspect its terminal
  diagnostics before preparing the next integration snapshot with these files.
  Full source-owned integration, detached-node diagnostics, permanent parity
  project wiring and syntax batch commit remain pending.


### Syntax completion gate: diagnostics and integration (2026-11-03)

- Author direction: continue until the syntax work is committed, then pause the
  active porting goal. Do not pause before the commit or mark the full compiler
  port complete. The objective remains broader than this syntax milestone.
- Corrected public compilation-unit factory parsing to retain its source tree and
  diagnostic locations. Regression testing exposed zero-width EOF diagnostics
  being omitted from span queries; both SyntaxTree implementations now include
  diagnostic markers at either boundary without changing general TextSpan overlap.
  The class case also exposed an absent missing-closing-brace diagnostic, now
  reported by both type parsers while preserving source and missing-token shape.
- Detached node diagnostics now preserve immutable SourceText/path context on
  diagnostic copies attached by node-only parser entry points. DiagnosticInfo
  converts this context to accurate external-file locations (SourceTree remains
  null), including nonzero input offsets. Nonrecursive node queries expose directly
  attached green diagnostics. Incremental ParseResult diagnostics remain separate.
  Focused factory/tree/parser/coordinate/printer validation passed 174 checks
  (`/tmp/detached-diagnostics-tests.log`). New regressions failed before corrections.
- First full integration snapshot (1,116 sources, session 47716) is terminal with
  two RAV0103 errors in static local functions calling siblings in submission
  completeness. The reduced runtime case failed before the shared binder fix.
  Static local lookup now admits explicitly static siblings across the scope
  boundary while still excluding captured state/nonstatic siblings. Fifteen
  focused/adjacent checks pass (`/tmp/static-sibling-tests.log`). The Raven binder
  is not ported yet; this shared C# correction supplies the bootstrap compiler.
- Started the next immutable snapshot: 1,118 sources, session 20216,
  `/tmp/raven-syntax-public-integration-v2-snapshot`, seed
  `/tmp/raven-syntax-public-integration-v2-seed`, output
  `/tmp/raven-syntax-runtime-public-integration-v2/Raven.CodeAnalysis.Raven.dll`,
  log `/tmp/syntax-api-public-integration-v2-emit.log`. It includes the printer,
  diagnostic changes and corrected static-sibling bootstrap. Poll the existing
  session/process until terminal rather than restarting on an observation timeout.
- Wired complete syntax-tree generation into the permanent Raven project, using
  intermediate generated files and an explicit bootstrap symbol-contract reference.
  MSBuild input validation reports 1,118 unique compile files (989 generated) and
  no duplicate legacy kind/facts inputs (`/tmp/syntax-port-project-inputs.log`).
  Shared syntax/parser tests and assembly-ownership checks now belong to the
  permanent CompilerParity projects. C# passes 875 checks
  (`/tmp/permanent-syntax-parity-csharp.log`). Permanent Raven project validation
  is still pending the current snapshot result and actual project build.
- Repository baseline stopped on AttributedCustomUnionTests (struct case): the
  documentation constructor xref points to method__ctor.html instead of index.html
  (`/tmp/syntax-final-repository-baseline.log`). This fixture invokes documentation
  generation. Do not claim a green full baseline or a verified pre-existing failure.
  A compiler-only continuation excludes the three documentation-generator test
  classes to honor the porting constraint: session 93275,
  `/tmp/syntax-compiler-baseline-no-docs.log`; exact exclusions are in
  `/tmp/syntax-baseline-doc-exclusions.txt`. Generator parity runs separately in
  session 59885 (`/tmp/syntax-final-generator-parity.log`).
- Remaining gates: inspect v2 diagnostics, fix integration/runtime defects, run
  the actual permanent Raven project/consumer and generator checks, inspect broad
  compiler results, finish docs/checklist/formatting, commit the syntax batch,
  then explicitly pause the active goal as requested. Native neoCLR execution
  has not been validated.


### Syntax commit gate: quoter and final integration (2026-11-03)

- The 1,118-source v2 snapshot emitted successfully in 1,729.4 seconds with
  1,983 warnings. Its C# consumer passed 827/875 checks; 48 traversal-dependent
  checks exposed iterator address-taking that wrote temporary locals while
  subsequent yields read hoisted fields. The initial TryParse/out reduction hit framework projection diagnostics, so
  a source-defined out method is used to isolate shared iterator lowering.
- RavenQuoter and options are now translated. The separately emitted component
  passed 12 C# checks, including explicit assembly ownership. These tests still
  use bootstrap-owned nodes; full project ownership remains the final gate.
- Quoter stabilization fixed nested-type static lookup, local-function parameter
  shadowing, nested return-type inference, tuple capture traversal and protected
  loop exits. A generic-array delegate regression exposed a persisted Reflection.Emit
  definition-lookup limitation; a CLI reflection adapter retains the actual delegate
  signature. The 59 focused/adjacent runtime checks pass before the iterator fix.
- Generator parity completed: 69 C# checks and 78 Raven checks passed.
- The compiler-only baseline hit a stale macro-expansion test expecting the old
  attribute spelling. Its assertion now expects `#[ComponentBoundary]`, and focused
  macro tests pass. The earlier RavenDoc link failure remains separately recorded;
  documentation generation is excluded from the follow-up compiler baseline.
- The actual repository project parity script is running with quoter, full syntax
  generation and the current compiler, logged at /tmp/syntax-final-project-parity.log.
  A passing emission alone is not acceptance: the C# tests must execute against
  the Raven-owned runtime before committing and honoring the requested pause.


Final-gate update:

- Permanent C# parity: 886 passed, including the quoter tests.
- Changed runtime slices: 120 passed in four isolated batches; adjacent loop/goto/
  disposal coverage: 28 passed. Iterator-focused coverage: 39 passed, including a
  source-defined out-method regression that isolates hoisted storage from optional
  framework projections.
- `dotnet format whitespace Raven.sln --include <128 touched C# files> --no-restore`
  completed successfully; diff whitespace checks pass. No analyzer/style rewrite.
- The first actual Raven project invocation was deliberately terminated before
  completion when its default documentation-generation setting was noticed.
  The port project now disables Raven/XML/Markdown documentation generation.
  The final Raven invocation is logged at /tmp/syntax-final-raven-project-parity.log
  with those settings also passed explicitly. This restart is not a successful
  emission result. Historical component snapshots remain independently recorded.
- The compiler-only follow-up baseline excludes the three documentation-generating
  test classes. Dependency builds still generated Core/Macros intermediate docs
  before this setting was noticed; no site generation was requested or performed.


The broader compiler baseline exposed one more shared defect in constrained generic
match recursion. It reproduces with the earlier v2 seed as well as the current
compiler: `Evaluate(left)` and `Evaluate(right)` failed to infer the enclosing T.
Ordinary-argument inference now admits that identity bound, while contextual
callback hints keep the existing placeholder exclusion. The existing regression
and 123 focused generic/closure/scope checks pass. The affected 8-class baseline
batch is being rerun; remaining classes resume separately rather than repeating
already-passing batches. Logs: /tmp/recursive-inference-tests.log,
/tmp/syntax-match-batch-fixed.log and /tmp/syntax-final-compiler-baseline-rest.log.

Public quoter parameter names also retain the C# named-argument contract. The
updated component passes 13 checks, and C# parity passes 887. The full Raven
invocation was restarted once more before emission to include those final source
names. The actual project build uses the checkout seed already containing all
syntax emission fixes; the later recursive-inference correction is separately
validated against the C# bootstrap. Public exported syntax-type inventory is now
also checked against the seed in the permanent Raven parity project.


Additional validation at the final gate:

- Overload-resolution feature suite: 439 passed using the current built test
  assembly (same repository suite filter, no dependency rebuild or docs generation).
- The latest reduced scope suite passes 11 cases, including plain and mixed-branch
  recursive iterator traversal and a recursive generic runtime call.
- TextMate regression script passes. Macro-expansion LSP tests pass all 18 checks
  after rebuilding current net10.0 dependencies with documentation generation disabled.
- The temporary v2 traversal probe confirms direct child nodes are valid while
  iterator-produced child nodes are null in that earlier artifact. The new iterator
  address rewrite is therefore necessary for full-runtime parity, beyond emission.


The actual project gate caught an evaluation-order defect, not 469 independent
syntax failures. Build-time IncludePortSyntaxTree added generated files after
Raven.Language.targets, but standalone compiler evaluation removed obj inputs
and reintroduced the legacy kind/facts files. It selected only 134 sources and
failed with cascading missing-member diagnostics. The project now uses explicit
SDK imports and declares its generated Compile items after SDK/language targets.
Design-time evaluation and the build target now select the identical 1,120 unique,
existing sources, including all 989 generated tree files and excluding legacy
kind/facts inputs. Evidence: /tmp/syntax-project-evaluation-after.json and
/tmp/syntax-project-build-inputs-after.json. The failed project run is retained at
/tmp/syntax-final-raven-project-parity.log; a fresh run with the corrected source
set and latest seed remains required before acceptance.


The compiler baseline's union-import ambiguity fixture placed imports after
namespace declarations. The source-order recovery fix intentionally preserves
those misplaced directives as recovery syntax, so the fixture no longer imported
its cases. Imports now precede declarations and the test asserts clean syntax
before checking semantic ambiguity. The focused test passes; the entire affected
union batch and final baseline tail are being rerun. This is a fixture correction,
not a relaxation of directive ordering. Logs: /tmp/syntax-union-import-fixed.log,
/tmp/syntax-union-batch-fixed.log and /tmp/syntax-final-compiler-baseline-tail.log.

The corrected permanent Raven project build uses the frozen latest compiler
at /tmp/syntax-final-project-seed (DLL hashes in seed-hashes.json). Its log is
/tmp/syntax-final-corrected-project-parity.log. All 1,120 source hashes are recorded
in artifacts/compiler-parity/syntax-source-manifest.json. Documentation generation
is disabled, and build/standalone evaluation select the same generated source set.


Compiler-only baseline completion: 7,849 checks pass across 87 batches after
replacing the two initially failing batch results with successful reruns (281
match/macro checks and 265 union checks). The continuation and final tail completed
successfully. This retains the baseline's existing heavy/runtime/sample exclusions
and additionally excludes AttributedCustomUnionTests, NamespaceDocumentationTests
and RavenDocGenerationTests to avoid documentation generation. The initial full
baseline's RavenDoc xref failure remains recorded and is not claimed as resolved.
The final Raven project build remains the only acceptance gate outstanding.


The complete permanent Raven project compiled all 1,120 inputs and passed 888 of
893 tests. Five submission-completeness cases exposed iterator pattern bindings:
the match wrote a temporary local while resumed reads used a hoisted field.
MethodBodyGenerator now resolves iterator-owned fields for pattern assignment,
just as it already did async fields. A focused runtime test reads a matched string
across two yields; all 12 syntax-port scope cases pass. The first focused build
already included the fix (its log name ends in before but is not a failing baseline).
The full failing parity run provides the before evidence. The frozen corrected
compiler is /tmp/syntax-pattern-final-seed; the final project rerun is recorded in
/tmp/syntax-pattern-final-project-parity.log. This build again generated dependency
Core/Macros XML despite command properties; no RavenDoc/site generation was
requested, and the port project itself keeps documentation generation disabled.

The frozen-before regression run confirms 11 passes and one NullReferenceException
in the new iterator-pattern case; the corrected compiler passes all 12. Adjacent
iterator closure/cancellation coverage brings the corrected focused total to 23.
Logs: /tmp/syntax-iterator-pattern-frozen-before.log and
/tmp/syntax-iterator-pattern-adjacent.log. No Raven source workaround was needed.


Final syntax API acceptance (2026-11-03): the permanent Raven project built the
complete source set with the corrected frozen compiler and passed all 893 tests,
with no failures or skips. This includes the five formerly failing submission
cases, public-type inventory/ownership, syntax traversal and the quoter. The test
results are artifacts/compiler-parity/Raven/results.trx; the build/test log is
/tmp/syntax-pattern-final-project-parity.log. The C# parity project passed 887 tests;
the generator parity projects passed 69 C# and 78 Raven tests. The compiler-only
baseline passed 7,849 checks with the documented exclusions and corrected reruns.
Focused runtime, overload-resolution, language-server and TextMate validation is
recorded above. The final iterator correction passed 23 focused/adjacent runtime
checks and demonstrably fails its new regression on the previous frozen compiler.

Validation executes modern .NET only. It does not establish native neoCLR,
.NET Framework or NanoFramework execution. The earlier RavenDoc xref failure is
still unresolved and explicitly excluded from compiler-only acceptance. No website
publication was performed. C# remains the bootstrap implementation; semantic symbol
contracts remain explicit dependencies of the Raven syntax project. Commit this
syntax slice and pause the broader porting goal at the author's requested boundary.


### Symbol display configuration slice (2026-11-03)

After syntax acceptance and sync, the next dependency boundary is the semantic
symbol API. Start with its independent SymbolDisplayFormat configuration and
option enums, then port the symbol contracts with the coordinated I-prefix
migration required by AGENTS.md. Symbol interfaces have not been renamed in this
preparatory slice; neither mixed interface APIs nor bootstrap-owned implementations
are being presented as a completed semantic port.

SymbolDisplayFormat is translated directly, including all presets and With methods.
Both implementations now have explicit value equality across all eleven options
and exact runtime type, while retaining reference representation and private
setters. Raven uses val/private-set properties with explicit default values.
Records were evaluated: the current primary-parameter-only storage restriction
would reshape this API, so this class implements the value contract explicitly.
See ../symbol-display.md for the contract and test boundary.

The focused baseline passed 47 existing display tests. The port then exposed a CLI
bug: static and instance field/property object initializers discarded their member
assignments. Four reduced runtime cases failed (expected42, actual0). The CLI
constructor driver emitted the raw shared field plan; it now applies Lowerer first,
as the portable constructor path already does. No Raven source workaround is used.
The post-fix focused display/constructor run passed78 tests; isolated Raven value
and cross-implementation preset/enum parity passed6 tests. Final permanent project
parity is running. Logs: /tmp/storage-initializer-before.log,
/tmp/symbol-display-initializers-after.log, /tmp/symbol-display-port-parity-after.log,
/tmp/symbol-display-full-csharp-parity.log and /tmp/symbol-display-full-raven-parity.log.


Symbol display slice acceptance: the permanent Raven project built successfully
in 1,683.5 seconds and passed all 899 tests, with no failures or skips. The
permanent C# parity project passed all 892 tests. The six isolated Raven checks
cover value semantics, cloning/accessor boundaries and differential preset/enum
configuration; the 78 focused C# tests cover display behavior and adjacent
constructor initialization. The full Raven build reports compiler warnings, including explicit non-null
assertions in the translation; this is not a warning-free qualification. No RavenDoc or website
generation was run for this slice. Native neoCLR execution was not tested.
The slice is ready for its own commit; the broader compiler port remains active.


### Documentation value dependencies (2026-11-03)

The next semantic-contract prerequisite is DocumentationComment, DocumentationTag
and DocumentationTagKind. All three now have direct Raven translations. Tags
remain record classes. Comments retain their private construction boundary and
explicit structural value equality, including tag elements; the internal mutable
builder retains identity. Both codebases use range-based blank-line trimming
instead of repeatedly shifting a list. The unused prefix helper and unused tab
local were removed from C# rather than copied into Raven.

The first translation used sealed record, which denotes a closed abstract hierarchy
in Raven. Corrected it to an ordinary record declaration, preserving C# sealed
record metadata; this was a translation correction, not a compiler bug. The
component test asserts both comment and tag classes are sealed and checks record
copy, equality and deconstruction behavior.

The baseline passed 20 existing documentation tests. After the changes, 45 focused
C# tests pass, including adjacent semantic attachment/display cases. All 25 new
shared value/parsing cases pass against the isolated Raven component. That component
uses bootstrap-owned SyntaxTrivia/DocumentationFormat dependencies (explicit Port
alias in the temporary harness), so it is not evidence of full Raven ownership.
The permanent C# parity project passes 926 tests; full Raven project parity and
ownership checks are running before commit. No RavenDoc/site generation was run.
Logs: /tmp/documentation-port-baseline.log, /tmp/documentation-csharp-after.log,
/tmp/documentation-component-parity.log, /tmp/documentation-full-csharp-parity.log,
and /tmp/documentation-full-raven-parity.log. The frozen integration compiler is
/tmp/documentation-port-seed, with DLL hashes recorded in seed-hashes.json.


Documentation slice acceptance: the permanent Raven compiler project built and
passed all 933 parity tests, with zero failures or skips, including ownership
checks for all three documentation types. The permanent C# project passed926
tests; focused C# documentation and adjacent semantic/display coverage passed45,
and isolated Raven component coverage passed25. The build completed successfully
with compiler warnings; this is not a warning-free qualification. Validation
targeted modern .NET; native neoCLR execution was not tested. No RavenDoc or
website generation was run. This slice is committed independently before the
coordinated symbol-contract migration; the broader compiler port remains active.


### Separate symbol naming pass (2026-11-03)

At the author's request, symbol naming is a separate pass before translating the
symbol implementations. The prechange compiler-only baseline passed 7,844 tests
across 88 batches, excluding runtime-heavy/development and documentation generation
suites as recorded in /tmp/symbol-contract-baseline.sh and its matching log.
The Roslyn-aware rename covered compiler and consumer projects, followed by an
audit of embedded Raven source, generator templates and target-specific clients.
See ../api/symbol-contracts.md for the migration contract. Validation is in progress.
The separately reproduced nested macro canonical-name defect is not changed in
this naming pass; retain it as the next stabilization follow-up.

Naming-pass validation so far: the required build script completed; both modern
.NET compiler/driver frameworks build, as do the neoCLR metadata importer/probe,
playground worker, developer CLI, headless language-service tool and integration
test project. The neoCLR build used the independent metadata project in
/Users/robert/Projects/neoclr-raven-port; native runtime execution is not claimed.
The focused naming suite passed 31 tests, C# compiler parity passed 926, generator
parity passed 69 C# and 78 Raven tests, and selected language-service completion,
definition and rename coverage passed 26 tests. Full Raven compiler parity and
the post-rename compiler-only baseline remain pending. Compiler Core/Macros builds
emitted XML/Markdown documentation into ignored build outputs despite the disable
properties; no RavenDoc site generation was invoked. Logs use the prefix
/tmp/symbol-name- (focused, csharp-parity, generator-csharp, generator-raven,
lsp-tests, neoclr-build, playground-build, headless-build, integration-build,
cli-build and testapp-build).

The public/protected metadata audit compares 10,513 rows before and after the
naming pass, with no additions or removals after normalizing the intended type
names and C# extension-block implementation hashes by receiver. It compares
member signatures, accessibility/type flags, inheritance and generic constraints;
it is not an execution proof. See /tmp/raven-symbol-api-audit and
/tmp/symbol-name-api-audit.log. The broader baseline stopped at batch 36 on two
label tests whose reflection helper still looked up the old concrete type name.
Updating that string restored all 33 focused label/naming checks. The first 35
batches passed 3,216 tests; the failing batch and remaining batches are being
rerun with /tmp/symbol-name-baseline-resume.sh. Compiler sources did not change
for this correction, and the full Raven parity build continues unchanged.

Symbol naming acceptance: all 88 compiler-only baseline batches passed, totaling
7,875 tests (3,216 + 761 + 3,898 across the original and resumed runs). Batch 43
initially lacked the compiler-generated XML sidecar because documentation emission
had been disabled; restoring that build fixture passed all 20 focused metadata
reference tests and the repeated batch. This required no compiler behavior change
and did not run RavenDoc. Full Raven compiler parity passed all 933 tests with no
failures or skips; C# parity passed 926. Both generator implementations passed
(69 C#, 78 Raven), and 26 selected language-service tests passed. The default
.NET macro-capabilities sample ran with the expected `capability functions` output,
covering the renamed symbol contract in generated macro adapters. The Raven build
still reports compiler warnings; this is not a warning-free qualification.

Final logs: /tmp/symbol-name-raven-parity.log, /tmp/symbol-name-baseline-after.log,
/tmp/symbol-name-baseline-resume.log, /tmp/symbol-name-baseline-resume43.log,
/tmp/symbol-name-metadata-recheck.log and /tmp/symbol-name-macro-sample.log.
The independent naming pass is accepted for its own commit. Actual symbol
implementation porting and the shared macro canonical-name correction remain
separate follow-ups. The broader compiler-port objective remains active.


### Macro canonical names during symbol review (2026-11-03)

A reduced semantic API probe found that a source macro `Identity` inside
`Outer.Inner` reported `Inner.Identity`: the default `MacroSymbol.CanonicalName`
used short display formatting. The shared contract now uses the namespace's
complete metadata name, preserving every segment independently of display
options. The same defect and correction apply to .NET namespaces and neoCLR
logical modules. The synthesized provider symbol already receives its complete
canonical name explicitly; no duplicate target implementation needs changing.

The focused baseline passed 36 existing macro naming/symbol tests. Eight new
C# regressions cover global, single-segment and nested scopes on both target
semantic paths; four nested cases failed before the fix. Afterward, all 138
regression and adjacent naming, symbol, expansion and completion tests passed.
The compiler and driver built for net11.0. NeoCLR coverage uses host metadata
references and semantic queries, not native runtime execution. No RavenDoc or
website generation was run; no syntax or grammar rules changed.

The Raven symbol contracts have not yet been translated: the current Raven port
uses the bootstrap contract, and its later direct translation must retain this
correction. This is a separate stabilization commit after the naming-only pass.
Logs: /tmp/macro-canonical-baseline.log, /tmp/macro-canonical-before.log,
/tmp/macro-canonical-build.log and /tmp/macro-canonical-after.log.

### Default interface bodies during symbol translation (2026-11-03)

The direct draft of the 30 symbol interfaces and 10 associated enums exposed
unqualified inherited-member lookup failures. The shared binder walked only
`BaseType`, missing the separate interface inheritance graph. It now walks the
constructed base interfaces for interface scopes, including diamond ancestors,
without exposing default interface members as ordinary class members.

Runtime regression coverage then exposed expression-bodied property/indexer
getters being emitted as abstract interface methods. CLI method body recognition
now includes both declaration forms. `SourceCallablePlan` already recognizes
these bodies for portable lowering; no equivalent portable change is required.
Neither binder nor method emitter has been translated to Raven yet, so these
fixes live in the maintained C# bootstrap and must survive their direct port.

Validation: 75 adjacent interface tests passed before the change; the reduced
unqualified property/method repro failed with RAV0103. After the lookup fix,
.NET runtime validation exposed a TypeLoadException for the abstract getter.
After both fixes, 86 focused and adjacent tests pass. Coverage includes generic
diamond lookup, semantic symbol/type queries, .NET runtime dispatch, inaccessible
private helpers, and class lookup boundaries. The neoCLR case checks semantic
binding with host references only, not native execution. The compiler core and
driver built for net11.0; touched C# files passed whitespace formatting. No
RavenDoc end-to-end generation was run. The normal test dependency build did
build RavenDoc and emit compiler-owned XML/Markdown sidecars for Core/Macros;
subsequent focused builds reused dependencies.

The symbol-contract translation remains a draft, not an integrated replacement.
Its next checks concern nullable property patterns and expression results, then
the symbol/display dependency boundary and shared implementation tests. Existing
Raven syntax, text and documentation components remain in place.

Logs: /tmp/interface-lookup-baseline.log, /tmp/interface-lookup-before.log,
/tmp/interface-lookup-after.log, /tmp/interface-body-after.log. The temporary
translation is /tmp/raven-symbol-contract-port/Symbol.rvn, with its latest
compiler diagnostics in /tmp/symbol-contract-port-second.log.

### Nullable whole-property captures during symbol translation (2026-11-03)

The valid Raven translation of the canonical-name getter uses `if let { ... }
containingNamespace = ContainingNamespace`. Its successful whole-value capture
incorrectly retained the nullable input type. Property captures now infer the
non-nullable matched type. The same regression tests exposed two adjacent CLI
failures: empty property patterns skipped capture assignment, and nullable value
inputs could not infer their property receiver. Empty captures now receive the
matched value, nullable structs use their underlying receiver type, and CLI
member extraction unboxes and addresses value receivers correctly.

The shared binder correction applies to both targets. Portable property-pattern
lowering already assigns whole captures and uses value receiver addresses in its
supported cases; its existing capability restrictions are unchanged. Native
nullable-value execution is not claimed. These binder/emitter sources have not
yet been ported, so the fixes remain in the C# bootstrap pending direct translation.

Validation: 26 focused tests passed before editing; all six new regression cases
failed before the fix. Final focused/adjacent coverage passes 125 tests, including
CLI runtime behavior for reference and value captures, null rejection, semantic
capture types, and existing portable/shared pattern tests. NeoCLR regressions
exercise semantic binding with host references. Core and driver net11.0 builds
pass. No RavenDoc generation was run.

The draft contracts now pass semantic analysis far enough to reach emission.
The next reproduced failure is collection-spread emission in `GetTypeMembers`
(`Unexpected method kind` resolving a substituted constructor). Two false-positive
unused-result analyzer warnings on nested conditional expressions also remain;
a reduced executable returns the expected lengths 1, 3, and 0. Neither issue is
worked around in the direct translation.

Logs: /tmp/property-capture-baseline.log, /tmp/property-capture-before.log,
/tmp/property-capture-after2.log, /tmp/symbol-contract-port-fourth.log and
/tmp/contract-conditional.log. Draft: /tmp/raven-symbol-contract-port/Symbol.rvn.

### Collection-builder spreads with source element types (2026-11-03)

The contract draft's `GetTypeMembers` spread into `ImmutableArray<NamedTypeSymbol>`
reached a reflection fallback that could not inspect the still-emitting source
interface type. Span-based collection-builder spreads now bind their factory
invocation in the shared binder, with element conversion checks, before emission.
This uses the ordinary call and array-to-span machinery for source element types.

Runtime tests exposed a second defect: normal-form `ReadOnlySpan<T>` arguments to
`params` factories became bound errors and emitted default spans. Argument
conversion now checks the supplied argument's normal conversion before requiring
an expanded-params element type. The direct-call and collection-spread regressions
both preserve their elements at runtime. These binder sources remain C# bootstrap
code until their dependency-ordered Raven translation.

Baseline: 39 passed and one stale JSON test failed because it accessed a nullable
deserialization result without unwrapping. That test now explicitly pattern-binds
the object or throws. Both new source-element runtime cases reproduced the
original emission crash. Final collection, builder, params, and conditional-
collection coverage passes 86 tests. Validation includes modern .NET runtime
execution, incompatible element diagnostics, and neoCLR semantic binding with
host references only. Span metadata/runtime support on other targets remains
required; native execution is not claimed. Core and driver net11.0 builds and
whitespace formatting pass. No RavenDoc generation was run.

The full contract draft advances to a separate emission failure in a lambda
inside the default `GetTypeMembers(name, arity)` implementation (`Method body
should not exist`); the same failure was also observed in `TypeSymbol.IsUnion`
during the intermediate investigation.
The nested conditional-expression analyzer false positives remain outstanding.
The translation is still a draft, not yet an integrated replacement assembly.

Logs: /tmp/contract-spread-baseline.log, /tmp/contract-spread-before.log,
/tmp/contract-spread-after2.log, /tmp/symbol-contract-port-sixth.log.

### Lambdas in default interface implementations (2026-11-03)

CLI method generation used the enclosing source interface to classify lambda
methods, although their emitted owner is a generated closure class. Those methods
incorrectly received abstract flags, making emission fail. Method definition and
body eligibility now consult the emitted owner. The correction applies to the
shared CLI emitter; it does not establish native closure support. The emitter has
not yet been ported to Raven and remains part of the C# bootstrap.

Validation: 49 interface tests passed before editing; three new runtime cases
failed before the fix. All 64 focused interface/closure tests now pass, covering
non-capturing lambdas, captured parameters, and captured interface receivers in
default getters/methods. Core and driver net11.0 builds and whitespace formatting
pass. No RavenDoc generation was run.

The isolated draft of all 30 symbol interfaces and 10 related enums now compiles
as a .NET assembly. This is not yet a completed integrated port: dependencies,
assembly identity, and shared C# implementation tests remain to be validated.
Two false-positive unused-result warnings in the property-parameter default
implementation are the next stabilization work.

Logs: /tmp/interface-lambda-baseline.log, /tmp/interface-lambda-before.log,
/tmp/interface-lambda-after.log and /tmp/symbol-contract-port-seventh.log.

### Consumed conditional results in default implementations (2026-11-03)

The translated `PropertySymbol.Parameters` default returned the right values but
reported RAV9034/RAV9026 for nested conditional results. The shared returned-value
analysis now follows branch values into expression-bodied members and enclosing
returned expressions. Immutable collection result analysis reuses that analysis
instead of keeping a separate incomplete implementation. Async result-type
handling remains explicit; an adjacent async-lambda regression prevented an
intermediate change from suppressing a real unawaited-task warning.

Validation: 32 original result-analyzer tests passed before editing; both new
conditional regression tests failed before the fix. Final result-analyzer and
unawaited-task coverage passes 57 tests. The isolated 30-interface/10-enum draft
compiles without warnings. Earlier runtime reduction returned lengths 1, 3, and
0, confirming this was analyzer policy rather than runtime semantics. Core and
driver builds and whitespace formatting pass. No RavenDoc generation was run.
The analyzers are not yet translated, so these changes currently belong to the
C# bootstrap; shared semantic policy applies regardless of target.

Next: integrate the symbol contracts with their visitors, attribute data,
comparers and display dependencies, and add shared C# tests against both source
implementations. Isolated compilation does not prove that bootstrap/source
assembly identity or the full dependency boundary is correct.

Logs: /tmp/contract-analyzer-baseline.log, /tmp/contract-analyzer-before.log,
/tmp/contract-analyzer-after2.log and /tmp/symbol-contract-port-eighth.log.

### Symbol enum dependencies and constant initializer stabilization (2026-11-03)

A runtime load/public-signature audit of the isolated symbol-contract assembly
succeeded but found 15 bootstrap type dependencies. Integrating the interfaces
still requires visitors, attribute data, comparer and display dependencies;
compilation alone is not accepted as proof of a complete port.

The independent symbol/macro classification layer is being integrated first:
10 enums from Symbol.cs, SpecialType, and five macro classification enums. All
16 compile in isolation and match the C# names, numeric values, underlying types,
and Flags attributes. A permanent C# metadata parity test checks ownership by
the Raven assembly. Full component build and integrated parity are still pending;
these translations are not yet committed as a validated slice.

MacroInvocationTargets.AllSingleNode exposed an existing enum-initializer gap.
Initializers lacked the ordinary type-member lookup scope for prior constants,
and numeric constant evaluation did not use enum integral storage. Both paths
are corrected. Adjacent regression coverage also found missing complement
operator binding; `~EnumValue` now preserves its enum type, and constant folding
uses the underlying width/signedness. The binder/evaluator remain unported C#
bootstrap components and must retain these fixes when translated.

Validation: the original enum baseline passed 31 tests; five new cases failed
before the fix. Focused enum, overflow, checked-expression and constant narrowing
coverage passes 136 tests, including emitted constants and runtime complements
for byte, int, long and ulong enums. NeoCLR coverage is semantic with host
references only; native execution is not claimed. Touched files were whitespace
formatted. No RavenDoc generation was run.

The initial full Raven build was deliberately terminated after the isolated
compile proved its MacroInvocationTargets input invalid. The replacement build
uses frozen compiler /tmp/symbol-enum-port-seed-v2 and log
/tmp/symbol-enum-port-build2.log; do not restart it merely because it is slow.
Logs: /tmp/enum-flags-baseline.log, /tmp/enum-flags-before.log,
/tmp/enum-flags-after4.log, /tmp/enum-flags-final.log,
/tmp/symbol-enum-component-build2.log and /tmp/symbol-enum-component-parity.log.

### Typed constant value semantics (2026-11-03)

The next symbol dependency draft includes TypedConstant, AttributeData and
MacroParameterBinding. The isolated assembly compiles, but its remaining
bootstrap dependencies still prevent treating it as an integrated symbol port.

TypedConstant is now a readonly record struct in C#, with the equivalent Raven
primary-constructor record prepared in the dependency draft. Typed equality,
operators, init-only properties and deconstruction make its existing value role
explicit. Equality retains boxed-value and ImmutableArray storage semantics;
this does not introduce recursive array equality. Both implementations preserve
the old ToString output explicitly. WithType no longer unboxes and reboxes array
payloads. AttributeData and MacroParameterBinding have not changed equality
semantics; their eventual port still requires a deliberate identity/value review.

The permanent C# TypedConstantValueTests are reused against the isolated Raven
assembly: all eight pass in both implementations. The focused C# attribute and
constant set passes 40 tests after a 32-test baseline. Initial shared tests
caught the getter-only C# versus init-only Raven property mismatch; both record
APIs now expose init-only data consistently. No new language syntax or target
policy was introduced, and no native neoCLR execution is claimed. No RavenDoc
was run. The enum component build remains independent and in progress.

Logs: /tmp/typed-constant-baseline.log, /tmp/typed-constant-after2.log,
/tmp/typed-constant-raven-draft-tests2.log. Draft sources remain under
/tmp/raven-symbol-contract-port/dependencies; integrate them with the symbol
contract dependency closure, then link the permanent tests into the ordinary
Raven parity project. Temporary isolated validation is not that integration.

### Visitor preflight and documentation regression (2026-11-03)

The isolated symbol visitor prototype now owns both visitor types and all 29
visit methods per type. Eleven C# tests pass against the isolated Raven assembly:
the eight constant tests plus forwarding, overrides and entry-point dispatch
through the local symbol contract. Generator integration is still pending.

The full pre-generator baseline stopped in its fifth batch: 339 tests passed in
four batches, then 116 passed and one imported-union documentation assertion
failed. That metadata test unexpectedly included RavenDoc site generation;
this was an end-to-end documentation test inside the ordinary baseline.
The investigation intentionally used that specific end-to-end scenario. Imported
union constructor xrefs share an overload-group key; projected case constructors
and ordinary default constructors previously chose different destinations for
that key. Links now consistently use the visible ordinary constructor group,
or the union page when there is no such group. Five focused union documentation
cases pass, followed by a three-case rerun with the ordinary constructor declared
before projected constructors. No compiler language or runtime policy changed.

Logs: /tmp/symbol-visitors-baseline.log, /tmp/symbol-visitors-draft-tests2.log,
/tmp/union-xref-after2.log, /tmp/union-xref-final.log. The baseline needs resuming;
RavenDoc end-to-end cases should be explicitly classified and excluded from
routine porting validation. The independent enum build remains running.

### Generic interface method dispatch stabilization (2026-11-03)

Compiling a concrete implementation alongside the generated Raven symbol visitors
exposed a CLI implementation-matching defect: emission succeeded, but loading the
class failed because its generic Accept method was not mapped to the interface.
The contract and implementation type parameters have different owner identities.
TypeGenerator now checks method arity and static/instance shape, then substitutes
the contract method parameters by ordinal before comparing its signature. Shared
symbol equality remains unchanged. This also handles renamed method parameters,
constructed parameter types and generic/non-generic overloads.

The existing focused interface baseline passed 10 tests; four new direct/inherited
runtime cases failed before the fix. The final five new cases and adjacent
interface tests pass: 18 interface tests, plus a 22-test follow-up containing the
five cases, seven static-interface tests and ten collection-comprehension tests.
The visitor generator's three parser/runtime tests now pass as well. The required
codex-build.sh completed successfully. Validation is .NET runtime/metadata;
native neoCLR mapping ownership was reviewed, but native execution is not claimed.
The fix belongs to the unported CLI emitter and must be retained when translated.

Logs: /tmp/generic-visitor-interface-baseline.log,
/tmp/generic-visitor-interface-before.log, /tmp/generic-visitor-interface-after.log,
/tmp/generic-visitor-interface-final.log, /tmp/symbol-visitors-output-tests3.log.

### Raven symbol visitor generator output (2026-11-03)

BoundNodeGenerator now has an explicit --raven-symbol-visitors output-directory
mode. It uses the same symbol-interface discovery and visit-name mapping as C#,
while leaving the default C# output mode intact. The mode writes only its two
Raven visitor files and does not alter the C# files or incremental stamp. It does
not yet generate concrete Raven symbol partials or translate the generator itself.

The generator baseline passed 69 tests; all 72 tests pass after adding deterministic
Raven output/parser tests and a compiled runtime fixture. The CLI produced all 29
methods per visitor from the real repository contracts. A hash comparison verified
that the existing C# outputs and stamps were unchanged by the Raven mode. The
required codex-build.sh passed. The generated files also compile with the isolated
symbol dependency draft; production symbol integration remains unfinished.

Logs: /tmp/symbol-visitor-generator-baseline.log,
/tmp/symbol-visitor-generator-after.log, /tmp/symbol-visitors-cli.log,
/tmp/symbol-visitors-generated-draft.log, /tmp/symbol-visitors-codex-build.log.
The broad compiler baseline is continuing from batch 9 after replacing the stale
collection-builder bound-node assertion with public semantic API coverage.

### Integrated symbol classification enums (2026-11-03)

Sixteen enums are now owned by Raven.CodeAnalysis.Raven: SymbolKind,
Accessibility, MethodKind, IteratorMethodKind, RefKind, ScopedKind, TypeKind,
TypeParameterConstraintKind, VarianceKind, TypeParameterOwnerKind, SpecialType,
MacroKind, MacroApplicationKind, MacroInvocationTargets, MacroTarget and
MacroParameterRole. The C# bootstrap definitions remain alongside them unchanged.
The translation preserves public visibility, underlying types, member names,
numeric values and Flags attributes. Uncommented adjacent enum members use the
compact Raven formatting convention.

The full Raven component build completed successfully in 37m23s with 2,027
warnings and no errors. Sixteen permanent C# metadata tests verify the translated
types belong to the Raven assembly and match the bootstrap metadata. Complete
parity validation passed 926 C# tests and 949 Raven tests, using the resulting
component without rebuilding dependencies. This validates the integrated enum
slice, not a completed symbol implementation or native neoCLR execution.

Logs: /tmp/symbol-enum-port-build2.log, /tmp/symbol-enum-integrated-tests.log,
/tmp/symbol-enum-full-parity-CSharp.log, /tmp/symbol-enum-full-parity-Raven.log.

The next dependency draft now includes ImportedAssemblySymbol,
ResolvedAssemblyArtifact and twelve metadata-provider capability interfaces.
They compile with the local symbol interfaces, visitors and attribute/macro data.
Twelve isolated C# tests pass, including contract signature parity, out-parameter
and NotNullWhen metadata, and ownership checks ensuring provider signatures use
local symbol types. These files remain a draft outside the production component;
comparer/display dependencies and concrete symbol implementations still need
porting before integrating the complete symbol contract set.

### Integrated symbol data and metadata contracts (2026-11-03)

The next production source slice adds TypedConstant/TypedConstantKind,
AttributeData, MacroParameterBinding/MacroParameterSource,
ResolvedAssemblyArtifact, ImportedAssemblySymbol, twelve metadata-provider
capability interfaces and ParameterConversionKind. Each metadata contract keeps
its own file. The full Raven component build completed in 37m47s with 2,027
warnings and no errors.

Unported symbol interfaces deliberately remain bootstrap dependencies in these
signatures, as with the existing Location.MetadataModule port. The data types,
artifact value and capability interfaces themselves are Raven-owned; tests state
both sides of this ownership boundary explicitly. SyntaxReference and typed
constant arrays used by AttributeData must come from the Raven implementation.
The isolated full symbol draft separately verifies these same capability
signatures can use local symbol interfaces when that dependency set is integrated.

MacroParameterBinding is a value mapping, now represented as a sealed record class
in both source versions, retaining reference storage, internal construction and
its original string output. AttributeData keeps sealed reference identity for
bound attribute occurrences. Shared C# tests cover these decisions, argument
storage, immutable copies and artifact identity. The original helper baseline
passed 12 tests and the adjacent macro baseline passed 7; the updated helper and
macro set passes 19. Complete integrated parity passes 939 C# and 964 Raven
tests, including ownership checks. The ownership test accounts for reflection
returning the complete inherited interface closure. No native neoCLR execution
is claimed.

Frozen host: /tmp/symbol-data-port-seed/rvnc.dll. Full build log:
/tmp/symbol-data-port-build.log. Other logs: /tmp/symbol-data-isolated-build.log,
/tmp/symbol-data-csharp-after.log, /tmp/symbol-data-adjacent-tests.log.

### Indexed lookup stabilization and next metadata slice (2026-11-03)

The broader baseline exposed partial generic override binding that depended on
syntax-tree/query order. Indexed type lookup had recursively completed unrelated
member signatures while a base method still held its temporary signature.
Type-only lookup now declares the type without that completion; qualified
expression lookup explicitly completes source signatures for constructor/member
selection. Fourteen override-order cases and the source-constructor ownership
regressions pass. The combined focused metadata/lookup validation passed 73 tests.
The resumed baseline completed its remaining 20 batches (1,671 passing cases).
A fresh full core baseline is now running against the final fixes. These fixes belong
to shared, not-yet-ported binding; no native execution is claimed.

The next isolated metadata draft contains PEReflectionTypeIdentity and the
PETypeIdentity/PETypeParameterIdentity records. Comparing distinct generic
reflection definitions previously recursed through the same definitions; the
C# fix now compares nominal definitions before constructed arguments. The direct
Raven draft carries the fix, and 14 shared C# tests pass against both helper
implementations (the isolated C# run also includes one temporary bounded-recursion
repro). Tests cover separate load contexts, record equality/hash/copies and
deconstruction. Explicit Raven ToString methods preserve C# record string output;
Raven's synthesized output quotes strings. Null-guarded accesses use explicit
suppression under Raven's documented static-nullability rules.

The draft remains outside the production component until the preceding symbol
data slice finishes integration. Sources: /tmp/raven-reflection-identity-port.
Logs: /tmp/source-constructor-reflection-tests.log,
/tmp/pe-identity-csharp-tests2.log, /tmp/pe-identity-raven-tests2.log.

A further reflection-identity regression distinguished CLR vectors from rank-one
multidimensional arrays. The new test failed before the correction; both the C#
helper and the isolated Raven translation now retain IsSZArray as well as rank.
The main compiler metadata selection passed 53 tests, and the isolated Raven
metadata slice passed 14 shared tests. Commit: 2a863ff1b. Logs:
/tmp/pe-array-identity-before.log, /tmp/pe-array-identity-adjacent.log,
/tmp/pe-array-identity-raven-tests.log.

Validation while the symbol-data component build runs: C# component parity passed
939 tests; generator parity passed 72 C# and 78 Raven tests; Raven.Core passed 87,
Raven.Editor passed 4 and AsyncEntryDiffRunner passed 1. Dependency builds were
reused where available; the editor dependency needed a targeted build. The Raven
component build has now completed and all 964 Raven parity tests pass.


Author direction: Raven records will adopt C# record semantics, including stored
body state and additional constructors. The MetadataImportOptions draft exposed
the previous explicit restrictions (RAV0345/RAV0346); the language extension is a
separate language feature slice. Its pre-change record baseline passed 66 tests.
The implementation and remaining compatibility work are recorded below.


Record compatibility extension checkpoint (2026-11-03): stored body state and
constructors now compile; equality/hash/copy use fields rather than getters.
Generated display and empty deconstruction behavior match nine equivalent C#
programs. The focused record suite passes 65 tests; another 69 adjacent tests pass. The language reference and
website source now document these rules without running RavenDoc. Remaining C#
compatibility work is explicit in docs/compiler/record-inheritance-equality.md,
including typed equality customization, copy metadata/diagnostics and native
backend parity. MetadataImportOptions integration remains a later port slice.


Typed record equality follow-up: object equality and operators now call typed
Equals; generated reference records have EqualityContract and derived sealed
base-typed overrides. Two failing custom-equality regressions now pass. The
67-test record/portable cohort and 18-test differential/stored-state cohort pass.
Union payload coverage confirms custom record equality propagates. User direction:
transfer foundational fixes to affected unions and other features, and audit
IsReadOnlyAttribute metadata. The latter exposed pending record-struct source
classification and imported-method marker issues; address in a separate slice.

Metadata parity directive: relevant metadata emitted by C# (including optional
IsReadOnlyAttribute markers) is part of Raven's .NET compatibility, not merely
an optimization detail. Validate emitted and imported type/member metadata for
records, union carriers and case types alongside runtime behavior. Transfer fixes
to other affected features and target implementations where the same contract
applies.

Final combined validation: 329 record/union semantic, runtime, interoperability,
completion and portable-plan tests pass. A malformed nested union fixture was
corrected (missing closing parenthesis); the newly exposed generic annotation
conversion bug was reproduced with the prior frozen compiler and fixed separately
in 8c67db2cd. Its 14 focused nullable substitution/runtime tests also pass.

C# metadata probe (/tmp/csharp-readonly-oracle.log) confirms type-level
IsReadOnlyAttribute on readonly record structs and readonly ordinary structs;
mutable record structs mark generated getters, ToString, PrintMembers,
GetHashCode, Equals and Deconstruct individually. CompilerGeneratedAttribute also
appears on synthesized members. Match relevant metadata in the next slice, with
separate consideration for union carriers/cases and imported readonly methods.


Readonly metadata slice: a temporary codex/raven-readonly-metadata worktree isolates
implementation from the running port baseline. C# differential probes found missing
record struct type/member markers, missing generated method markers, and incorrect
readonly method import based on return modifiers. Four of five initial parity cases
failed. The implementation adds readonly record struct support, readonly/generated
member metadata, correct import, immutable union storage metadata and diagnostics
for ignored union case binding prefixes. Ten focused tests pass, including C# differential metadata and mutable getter
behavior. All 301 adjacent record/union/struct tests pass. The validated slice is
integrated into codex/raven-compiler-port as 80b21ece7. RavenDoc was not run.

The earlier full port baseline stopped with six nullable generic-inference failures
in GenericMethodTests after the shared conversion correction. A Roslyn probe
confirms Coalesce<T>(T? value, T fallback) called with int? and int infers T=int?,
not int. Raven currently unwraps the nullable argument and then rejects the call.
Fix inference and update the old int expectations against C# evidence next;
do not restore the spurious nullable-to-nonnullable conversion. This failure is
independent of readonly metadata and is not a clean full-baseline result.


Nullable inference follow-up: preserve an actual nullable value argument when
inferring an unconstrained annotated `T?`. The overload binder and cold available
semantic path now agree: the latter declines a partially inferred candidate when
known argument conversions cannot establish applicability. Source and imported
method tests cover query-first and diagnostics-first ordering. The 201-test cohort
covering generic invocation, nullable substitution/runtime behavior, contravariant
inference and incremental compilation reuse passes. All 439 overload-resolution
feature-suite tests also pass. This fixes the six failures
reported above; a complete new baseline is still pending. The inference and
semantic-model owners have not yet been ported, so the fix is in the shared C#
bootstrap compiler, with no divergent Raven implementation to update. Native
neoCLR execution was not tested.


Record copy metadata follow-up: the MetadataImportOptions draft now passes nine
C# tests against the Raven implementation, including copy-constructor metadata.
The bootstrap compiler synthesizes private copy constructors for closed records
and protected constructors for inheritable records. It determines the required
member contract from completed own/base members and emits the marker for generated
constructors without requiring authored syntax. Five Roslyn differential cases,
two access-diagnostic cases and a C# derived-record runtime interoperability case
pass; all 199 adjacent record/with/required-member/readonly/portable-plan tests pass.
The old direct-copy fixture now uses the public `with` operation. A separate test
cleanup retains zero-binding assertions while allowing cached semantic answers;
its prior cache-hit failure was reproduced on d592b0355 before this fix.

Validation used modern .NET. These are shared source-symbol contracts; native
neoCLR execution, .NET Framework and NanoFramework were not tested. The C# owners
have not yet been ported to Raven. MetadataImportOptions remains a validated draft
until its shared tests and source are integrated after the PE helper slice.


Workspace consolidation (author direction): keep porting and port-discovered
compiler stabilization in the single Raven-compiler-port worktree on
codex/raven-compiler-port. The temporary record-copy workspace has been removed;
3a1db06ce and 34bbd0a07 are integrated, and pending metadata-import options and
runtime-contract translations are consolidated beside the PE/readonly slice.
Continue committing separately validated slices within this worktree. Long-running
validation started before consolidation retains its original source/binary scope;
it is not validation of newly added port files.


Prepared compiler-option dependencies: MetadataImportOptions has twelve shared
value/metadata cases passing against both isolated implementations; its reorganized
compiler integration cohort passes 27 tests. Seven runtime-contract record types
(six files) retain their original defaults and have nine shared cases passing in
both implementations. AsyncInvestigationOptions is an immutable reference data
object, now a sealed record in C# and a record class in Raven. Its factories and
visibility are retained; the initial equality test failed against the old C# class.
The returned-value mode parser is translated directly. Their twenty shared cases
pass in each implementation, with ownership checks prepared for full-port parity.
Full consolidated-port validation is still required before committing these slices.

The parser translation preserves the documented nullability boundary: conditional
.NET flow attributes do not refine Raven storage locations. After the null/blank
guard, `value!` supplies the known non-null reference without adding a runtime check.
See docs/lang/nullability.md. Reconsidering conditional-flow interoperability with
C# is a separate language decision, not an unreported compiler workaround.

The previous PE/readonly full build completed with RAV0330 on AttributeLists in two
freestanding macro syntax classes, despite the overrides being present in generated
parts. This is under reduction. The core baseline stopped at the old cache-hit
assertion already corrected by 3a1db06ce; the new 44-test focused run, including
that cache query, option values and existing partial override tests, passes.


The consolidated follow-up changes the early abstract override-candidate guard to
inspect all partial declarations through their own semantic models. Forty-one
focused override/partial tests pass, including generated/handwritten file-order
coverage and a negative missing-property case. The simple reduced fixture also
passed before the change, so it does not independently reproduce the full-port
failure. The full consolidated build is the required confirmation before calling
that integration failure fixed. It uses a frozen compiler host and a recorded
Raven-source hash manifest; shared C# parity and a fresh baseline run alongside it.


The consolidated build completed in 22m40s without the previous abstract-property
errors, but failed on enum defaults in ExpressionSyntax.Missing and the operator
HashSet initializer. A small two-file bootstrap-reference regression reproduced
both errors in either file order. Indexed source enum symbols were available before
their constants, and exact member completion only handled nominal type declarations.
Completing the owning enum model fixes both focused cases; full-port retry and
adjacent validation are required before accepting the consolidated artifact.


The cold enum completion correction is committed as bb5a6b86b: 77 adjacent lookup,
enum and optional-parameter tests plus nine enum runtime cases pass. A frozen host
with that correction is compiling the consolidated port. The original baseline
reached Raven parity and launched a duplicate port build with the older host; that
specific compiler was stopped to prevent concurrent writes to the port artifact.
Its completed results are retained, and the remaining independent baseline projects
pass: C# generator parity 72, Raven generator parity 78, Raven.Core 87 and editor 4.
These validate modern .NET execution (net11.0 for parity, net10.0 for Core/editor),
not native neoCLR. Full port compilation and Raven compiler parity remain pending.


Stack inspection of the still-running full build exposed another local ownership
bug: RefTypeSymbol and NullableTypeSymbol registered type-use wrappers as namespace
declarations. Four source/metadata namespace regressions failed before disabling
that registration, and all 184 focused/adjacent tests pass afterward. The fix is
committed separately as 96966fc15. Both targets use these shared implementations;
no native execution is claimed. Carry the same non-declaration ownership into
both Raven wrappers when their symbol dependency boundary is ported. The active
full build retains its earlier frozen host and does not validate this later fix.


### Consolidated metadata and option slices accepted (2026-11-05)

The frozen-host build completed successfully in 33m57s with zero errors, followed
by all 1,032 Raven compiler-parity tests passing. The captured Raven source manifest
still matches the committed sources. This supersedes the pending full-port gates
above, including readonly record storage/attributes, PE helper ownership, metadata
import option constructors/copies, runtime-contract records and option helpers.
The shared C# parity baseline passed 1,005 tests; Raven adds ownership and generated
API checks. The warning count remains 2,070; acceptance does not mean warning-free.

Validated slices were committed separately:

- f900da8d3: partial-declaration abstract override candidates.
- 5e563d5a6: PE reflection identities and readonly record metadata in the port.
- 13e086367: metadata import options and shared value/metadata tests.
- 78462541f: seven runtime-contract record types, retaining target-owned policies.
- 8c39bb0fe: async investigation settings and returned-value mode helpers.

The build used the enum-completion fix bb5a6b86b. The later type-wrapper namespace
fix 96966fc15 has separate 184-test validation; it was not retroactively included
in the frozen compiler. The independent generator/Core/editor baseline remainder
also passed as recorded above. All validation is modern .NET; .NET stays the default
and this does not establish native neoCLR execution or complete compiler porting.

Next, continue the semantic symbol boundary in dependency order. Symbol interfaces
require their visitors and equality comparer; the comparer also depends on concrete
nullable, structural tuple/function, unit and constructed named-type semantics.
Do not substitute bootstrap adapters for those behaviors or declare the symbol API
fully ported before ownership and shared behavior tests cover that boundary.


### Symbol identity contract prerequisite (2026-11-05)

Committed d702d4f4e separates SymbolEqualityComparer from concrete nullable,
structural tuple/function and Unit implementations through four internal identity
provider contracts. Constructed generic equality reuses the existing substitution
contract. Identity facts, equality/hash algorithms and target-selected runtime
representations are unchanged. The four independent-provider cases failed before
the comparer adopted those contracts. Validation passes 51 focused tests, 62
adjacent structural/Unit cases, and 1,010 shared C# parity tests.

Direct Raven declarations for those four providers and the existing constructed
substitution contract are prepared. All five shared metadata-shape cases pass
against their isolated Raven assembly. A frozen-host full-port build and queued
Raven parity run now validate their integration; this host also includes the
previous type-wrapper namespace fix. The symbol interfaces, visitors and comparer
itself remain to be translated. These internal identity contracts are not a claim
that the symbol implementation boundary has already been ported.

### Symbol comparer translation stabilization (2026-11-05)

The isolated direct comparer/interface/visitor translation exposed two compiler
defects, fixed and committed independently before integrating that larger boundary:

- `4b459de20`: unrelated interfaces now admit explicit reference conversions and
  typed property patterns. All five regression cases failed before the correction;
  78 focused/adjacent tests and the 363-test pattern suite pass afterward. Both
  target configurations receive semantic checks; only modern .NET executes.
- `7c00503cd`: local functions inside interface default bodies retain their CLI
  method bodies instead of becoming abstract interface contracts. Four metadata/
  runtime regressions failed before the correction; 11 focused interface tests and
  14 adjacent local-function tests pass. A C# consumer that previously reported an
  unimplemented local helper now builds. The correction belongs to shared CLI
  emission, without claiming native neoCLR execution.

The isolated comparer now compiles and passes three existing C# behavior cases
(metadata-name distinctions, type-kind distinctions and cyclic containing symbols)
against its own translated symbol contracts. That small probe is not full comparer
coverage or integration acceptance. Compilation and method emission are not yet
ported; carry these corrections when their Raven counterparts are translated.

Commit `9a652f5f7` fixes a further reduced repro: escaped `@field` and backtick
`field` locals were wrongly bound as the backing-field keyword. Binding, storage
classification and semantic projection now inspect the spelling. Ten new cases
failed before the fix; 61 focused/adjacent cases pass afterward, including cold
queries, diagnostics-first queries and .NET accessor execution. NeoCLR receives
semantic-only host-reference coverage. The binder is not yet translated. Existing
escaped-identifier grammar remains applicable; no new token or syntax was added.

### Identity-contract port accepted (2026-11-05)

The integrated Raven build completed successfully in 34 minutes 11 seconds with
2,070 warnings and no errors. All 1,037 Raven parity tests pass against the captured
assembly, including the five internal identity-provider metadata cases. The source
manifest still matches the validated inputs. The C# parity baseline passed 1,010
cases. This accepts the five Raven identity contracts; it does not claim the public
symbol interfaces or comparer are integrated yet. The frozen build host includes
`d702d4f4e` and the earlier wrapper fix, but not the three subsequent stabilization
commits, which retain their separate evidence above.

### Public symbol boundary integration accepted (2026-11-05)

Thirty symbol interfaces, handwritten/generated visitors and the full equality
comparer are now translated in the port worktree. Symbol-bearing data signatures
must resolve to the port's own interfaces. The equality algorithms remain direct
translations; local input bindings accommodate immutable Raven parameters, and
escaped identifiers preserve keyword-named C# locals. The comparer keeps custom
reference identity for recursion keys.

The Raven project regenerates symbol visitors into its intermediate directory
using the existing BoundNodeGenerator Raven-output mode. The required generator/
build script passes. Shared C# contract tests exercise equality, nullability modes,
structural function identity, cyclic containment and visitor dispatch; interface
shape and recursive signature ownership checks accompany the Raven integration.
The final full build succeeded using the frozen compiler host and bootstrap
metadata reference. All 1,078 Raven parity cases and 1,020 C# parity cases pass,
including the shared comparer/visitor behavior tests, all 30 interface shapes and
recursive ownership of Raven signature types. The compiler's project evaluation
requires generated visitor files in the ordinary Compile item list as well as the
build target; this now follows the existing generated syntax-tree arrangement.
Concrete symbol implementations, Compilation and binding/emission services remain
future porting work.

The accepted build took 36 minutes 48 seconds with 2,073 existing warnings and no
errors. Its source manifest matches the committed Raven inputs without changes;
the tested assembly SHA-256 is
`e9101607d2743de8196e61f598548d3e72eb63501434993a0c1c15ff88976029`.
This validates .NET-hosted component parity, not self-hosting or native neoCLR
execution. The retry history and separately prepared next slices follow.

### Semantic result-value preparation and integration retry (2026-11-05)

The first public-symbol integration build terminated with exit code 1 after
18 minutes 22 seconds, without a compiler diagnostic or MSBuild failure summary.
It did not run parity tests. A diagnostic retry uses a single MSBuild node,
disabled build servers, a binary log and an explicitly frozen bootstrap metadata
reference as well as the frozen compiler host. The reason for the first failure
is not yet established; this is not acceptance of the symbol boundary.

Preparing the next semantic result-value slice found that `Conversion.None`
reported an explicit conversion. Commit `b011c4f07` corrects that flag and makes
Conversion readonly with IEquatable, retaining its custom symbol-aware value
semantics. Two new cases failed before the fix; 89 focused/adjacent C# tests pass.
Roslyn's classification of an impossible int-to-string conversion independently
confirms Exists/IsImplicit/IsExplicit are all false. Do not infer this from Roslyn's
default struct value, whose representation differs from Raven's None contract.

Commit `24ece74b5` preserves optional-symbol hashing explicitly in both prepared
implementations: zero for null, otherwise the semantic symbol hash. This local
cleanup avoids a contravariant nullable-comparer mismatch while preserving Raven's
existing stricter nullable variance direction. The 89 C# cases still pass.
Conversion's Raven source is only an isolated draft, not yet integrated. Its
parameterless constructor currently produces existing initialization warnings.

The draft also exposes an independent binding defect: a pattern variable in an
if-expression branch can be read as a member receiver but becomes unavailable in
a nested call argument, for example `HashCode.Add(if value is string text {
Length(text) } else { 0 })`. A standalone repro confirms RAV0103 without compiler
symbol dependencies. This scope defect must be fixed, not hidden by changing the
translation's algorithm. The conversion draft has not passed compilation/tests.


Conditional pattern stabilization follow-up: re-register cached condition pattern
locals for successful-branch binding, restoring the prior scope before else. Cold
ordinary-if identifier queries now use the same declaration-owned binding path as
if-let queries. The new regression cohort initially failed 8 of 16 cases; after
the scope correction, two cold-query identity cases still failed. Both corrections
now pass all 155 focused/adjacent cases, including six modern .NET runtime cases
and .NET/neoCLR semantic query-order cases. Native neoCLR execution is not claimed.
The full symbol-port retry remains live with unchanged input hashes and frozen
compiler/reference assemblies; this later compiler fix is not in that host.

The isolated Raven Conversion draft now compiles (13 existing definite-initialization
warnings) and passes the five unchanged C# ConversionValueTests. The probe retains
an aliased bootstrap reference for its symbol signatures, so this establishes value
behavior, not ownership of all ported API dependencies. Production integration and
full parity acceptance remain outstanding.

Accepted compiler fix: `3fa9ccec5` (conditional pattern scope and symbol identity).
All 365 broader syntax/semantic pattern tests also pass. Touched C# files were
whitespace-formatted; no parser, grammar, generator, or target-specific encoding
changes were required. The binder and SemanticModel have not yet been translated,
so this shared compiler fix lives in the retained C# bootstrap implementation.


Semantic-result preparation: `954072245` makes C# SymbolInfo a readonly record
struct, preserving constructors, default-vs-empty candidate arrays, ImmutableArray
storage equality and custom display. The new typed-equality regression failed
before; all 25 value/reason/pattern-symbol tests pass after, including operators.
The C# core builds cleanly and touched files are whitespace-formatted.

Direct Raven SymbolInfo and CandidateReason drafts remain outside production
inputs while the full symbol-port build runs. Two reduced compiler gaps prevent
acceptance: a record struct's `init(value: int) : self()` cannot resolve its implicit
zero-argument constructor (RAV1501), and an unqualified call to its own ToString
reports ambiguity with inherited ValueType.ToString (RAV0121). Both reproduce in
standalone tiny sources without compiler symbols. The SymbolInfo conditional
ImmutableArray construction also reports incompatible branch types; that case
still needs reduction before classifying it as a compiler defect. Do not remove
constructor delegation or replace normal override lookup to hide these failures.
No Raven SymbolInfo parity pass or production integration is claimed yet.


Accepted override lookup fix: `c41071c01`. Unqualified lookup now follows virtual
slot signatures across imported override chains, which lack source override links.
The minimal record-struct display probe compiles. Four new cases failed before;
all 34 focused/adjacent cases pass after, including ordinary/record struct .NET
execution, neoCLR semantic queries, distinct ToString(int) overload retention,
generic/covariant override coverage, class inheritance and record inheritance.
No native neoCLR execution is claimed. The shared binder is still C# bootstrap
code; the Raven SymbolInfo draft will consume this correction when integrated.
The implicit source-struct self() constructor and conditional immutable-array
inference issues remain outstanding; the full symbol-port build is still running.


Accepted implicit struct constructor fix: `4bed4523e`. Four regression cases failed
before; 56 focused/adjacent cases pass after. Struct and record-struct runtime tests
verify zeroed storage, explicit-constructor field initialization and absence of a
physical implicit CLI constructor. A C# oracle independently gives default field
value 0 versus 7 through the explicit delegating constructor. neoCLR semantic cases
and ordinary-struct portable-plan admission pass; configured neoCLR adapter build
passes. The existing modern .NET IEquatable allows-ref-struct constraint admission
limit remains explicitly covered by PositionalRecordPlanTests. Native execution
of the new cases remains unverified. The Raven SymbolInfo draft now has only the
conditional ImmutableArray branch-type failure, plus one initialization warning.

The full symbol-port retry completed in 35m08s, with 2073 warnings and zero errors;
all frozen source hashes matched. Its queued parity compilation failed because the
emitted SymbolVisitor types lacked VisitType. Evaluation evidence showed no generated
visitor sources in Compile items: inclusion existed only inside the build target.
The project now also includes the generated visitor glob during normal evaluation,
matching the established syntax-tree arrangement. Evaluation now lists both visitor
files. A new frozen-host/full-reference build is running with this correction; its
outputs and parity evidence must be accepted before committing the symbol slice.


The remaining SymbolInfo conditional-array failure is now classified as a deliberate
nullability difference, not a compiler inference bug: Raven keeps the original T?
static type after a null check (docs/lang/spec/type-system.md). Both constructor
implementations now explicitly pattern-bind the non-null Symbol before passing it
to ImmutableArray.Create. The C# implementation passes all 25 relevant tests. The
Raven SymbolInfo/CandidateReason drafts compile with one existing initialization
warning and pass all four unchanged SymbolInfoValueTests using aliased assembly
references. This isolated probe still consumes bootstrap Symbol contracts; full
port API ownership and production integration are outstanding.


Semantic-result preparation: `e4353937a` makes TypeInfo a record class, retaining
reference-type representation, its internal constructor and get-only fields while
adding value equality and record display. No derived TypeInfo classes were found
in the repository. The new typed equality case failed before (14 adjacent/control
cases passed); all 21 result/conversion/extension/cold-query cases pass after.
The direct Raven draft compiles without warnings and passes all three unchanged
TypeInfoValueTests against its emitted type. The isolated probe uses bootstrap
Conversion/TypeSymbol signatures; production integration and ownership checks are
still required. Conversion, SymbolInfo, CandidateReason and TypeInfo drafts are
ready for the next dependency slice after the running public-symbol acceptance.


Combined semantic-result dependency validation: all four drafts were compiled
in one Raven.SemanticResults assembly through the compiler API, referencing the
previous full port artifact's owned symbol contracts without the driver's automatic
C# compiler reference. The four drafts emit successfully and pass 14 C# tests:
five Conversion cases, four SymbolInfo cases, three TypeInfo cases, symbol-signature
ownership and CandidateReason name/value parity. Conversion.MethodSymbol and both
TypeInfo type properties now reference the ported symbol assembly; SymbolInfo's
symbol and candidate-array signatures do likewise. This strengthens the earlier
isolated probes, which still used bootstrap signatures. It is not acceptance of
the current full symbol build or production integration of the four drafts.

The next independent compiler-service slice after these result types is
DiagnosticBag and its generated reporting helpers. DiagnosticBag is a mutable
service, so reference identity is appropriate; its reporting scopes and concurrent
deduplication already have C# tests that can be shared. Generator support must retain
C# output while adding Raven reporting methods. Inspect scope disposal behavior
and preserve the current reporting contract during direct translation.


DiagnosticBag preparation: `f7375948f` makes non-reporting scope disposal atomic
and idempotent. The original concurrency baseline passed; both new repeated/concurrent
disposal cases failed before, while nested-scope and reporting controls passed.
All 46 focused/adjacent diagnostic tests pass after. The C# change atomically
releases the scope's bag reference before resuming reporting. The direct Raven
draft carries the same fix, keeps DiagnosticBag open and preserves explicit private
storage. Static lambdas use Raven's supported `static func` spelling.

At this preparation stage, two independent reduced defects blocked the draft
(both are resolved by the stabilization commits recorded below).
`Box<byte>(0)` returns Box<int> despite explicit type arguments, and an ordinary
byte parameter rejects the fitting literal 0, as does ConcurrentDictionary's
TryAdd value argument. The latter contradicts the documented implicit constant
conversion rule in docs/lang/spec/expressions-and-inference.md. Fix the compiler
rather than adding casts or byte suffixes to conceal these gaps. Generated Raven
reporting helpers have not yet been implemented; C# generator output is unchanged.

Explicit constructor type arguments: DiagnosticBag preparation also exposed a
shared binder defect: `Box<byte>(value)` with a `long` argument silently inferred
`Box<long>` after overload resolution failed. A focused regression failed for both
.NET and neoCLR semantic configurations before the fix (2 failed, 4 passed).
The fallback now respects explicit arguments; omitted arguments still participate
in inference. All 123 focused constructor, target-typing, generic-method and
union-generic tests pass after the fix, and the C# compiler builds cleanly.
This binder has not yet been ported; the correction is in the retained bootstrap
compiler. No native neoCLR execution is claimed.

Constant argument conversion preparation: the six valid `int`-constant-to-`byte`
argument cases failed in both target configurations before the fix (6 failures,
6 invalid-input controls passing). Ordinary literals now have primitive semantic
types, but argument conversion had retained a type-only path that lost constant
values. Shared expression-aware conversion classification reuses existing numeric
constant evaluation and conversion rules, retaining lexical checked/unchecked
context and primitive literal types. Focused validation passes 42 cases, including
.NET execution with imported ConcurrentDictionary methods, expanded params and
int-versus-byte overload selection; neoCLR validation is binding only.

The DiagnosticBag Raven draft now emits successfully with this compiler fix. The
isolated assembly passes all four existing C# concurrency/suppression tests plus
one dependency-ownership check. DiagnosticBag.Report takes the port-owned
Diagnostic type. These are preparation probes in `/tmp/diagnostic-bag-port`, not
production integration: reporting-helper generation and the full port acceptance
run still remain.

Constant argument stabilization is committed as `0dfa3b8dc`. Final validation:
48 focused conversion/API tests, 125 adjacent arithmetic tests, and all 439
overload-resolution suite cases pass. The initial broader run caught two
method-group regressions; overload applicability now preserves the resolved
natural function type for method groups while ordinary value expressions retain
constant values. Spec, compiler API documentation and changelog are updated.

DiagnosticBag preparation now also reuses three behavior tests for enumerable
construction and later deduplication, clearing only intersecting spans, and
retaining existing diagnostics during suppression. C# passes seven bag tests;
the isolated Raven draft passes those same seven plus dependency ownership
(eight total). These tests are committed before production port integration;
the shared parity project links will be added with that port slice.

### Semantic result integration pending stabilization (2026-11-05)

Conversion, SymbolInfo, CandidateReason and TypeInfo are now translated into the
production port directory. The final TypeInfo translation uses `open record class`
to preserve C# inheritance; a shared C# derived-record test checks equality and
base-typed copying. The isolated four-type Raven assembly passes all 15 behavior
and ownership cases. The C# parity project passes 1,033 cases with the 13 shared
result-value tests linked.

A new full-port build uses a frozen host containing the constructor and constant
argument stabilization fixes. It queues Raven parity with signature ownership and
readonly metadata checks after successful emission. This slice remains uncommitted
until that actual build and parity run complete. The accepted public-symbol slice
is commit `93f9804f7`; its 1,078 Raven and 1,020 C# cases are not evidence that the
new result-type integration has passed.

Diagnostic reporting preparation now generates all 366 descriptor-based helpers
from a draft DiagnosticsGenerator Raven mode. The generator preserves C# output
byte-for-byte and leaves production C# generation stamps untouched. Compiling
these helpers exposed nullable params-container overload selection: four reduced
source/imported .NET/neoCLR semantic cases failed before, while single-candidate
controls passed. Unwrapping the container annotation when resolving its params
element type aligns overload applicability with existing argument binding. All
23 focused cases (including .NET execution) and all 439 overload cases pass.

The 366 generated helpers now emit and produce matching C# diagnostic IDs,
messages, argument order and severity in an isolated probe (nine tests including
all bag behavior/concurrency tests and a reporter loop). The two handwritten
DiagnosticBagExtensions overloads are not yet translated; the ambiguity helper
depends on unported symbol-display code. Generator integration remains pending,
and this does not establish successful full semantic-result integration.

Reporting-generator support is committed as `44407c195`. Production Raven catalog
and reporter modes match the tested outputs and preserve C# files/stamps. The
required codex-build script passes. Generated reporters use the existing .NET
explicit ExtensionAttribute form in a partial static class, preserving receiver
parameter names and composing with handwritten overloads. All 366 generated
reporters match C# output with both non-null and null arguments, and the translated
handwritten severity overload passes: eleven isolated tests total.

The ambiguity-reporting overload remains pending because it requires full symbol
display behavior; do not substitute a simplified display string. DiagnosticBag
and helper production integration is separate from this generator commit. The
semantic-result build's Raven input manifest remains unchanged.

The full semantic-result build finished unsuccessfully after 23 minutes 27 seconds:
`BaseParseContext.LastToken` reports RAV0307 (no suitable property to override).
Raven parity did not run. A reduced base/derived property example passes with the
same frozen compiler host; full-context signature investigation is pending.
Do not treat the isolated 15 cases or prior accepted public-symbol results as
acceptance of these four integrated result types.

Symbol-display preparation is committed as `e9cb3027c`. Internal read-only
representation contracts replace concrete wrapper dependencies while retaining
existing display behavior. All 94 focused and adjacent cases and the required
build script pass. The tests include independent providers and existing neoCLR
structural-symbol semantics; runtime validation is .NET only. Translation of the
formatting implementation and new contracts remains a separate dependency slice.

Enclosing-namespace signature stabilization is committed as `18335bfb3`.
The reduced LastToken failure showed preliminary base-property types remaining
errors when full binding could resolve a nearer namespace type. Signature lookup
now checks enclosing scopes after local declarations/imports and does not jump
prematurely to a global type. The 39-case baseline passed; five of eight new .NET
cases failed before correction. Initial neoCLR cases failed fixture setup and
are not pre-fix semantic evidence. With the existing CLI-reference semantic-only
configuration, all 16 query/declaration-order cases pass across both targets,
as does a separate global-shadow property case. The 39 adjacent cases and 66
import/namespace cases pass. Core and driver builds pass; the original CLI
reproduction now succeeds. No native neoCLR execution is claimed.

A fresh full semantic-result integration build is running with a frozen compiler
host containing this correction. It will run Raven parity only after successful
emission. The four result types remain uncommitted until that validation succeeds.

An additional reduced gap remains: with a global `struct Token` and a nested
`Outer.Inner.Token` class, the latter's `static val Empty: Token = Token()` reports
RAV1504 (cannot assign Token to Token). This appears to be constructor-expression
lookup choosing the global type; inspect binding before fixing it. The property
signature regression does not conceal this with a changed port translation.

The constructor collision is now reduced and corrected in the shared block,
namespace and import binders (pending broad baseline before commit). A 32-case
pre-change baseline passed. Of 24 initial lexical-scope regressions, 20 failed
and four import-precedence controls passed. The final focused set passes 51 cases,
including .NET/neoCLR semantic lookup, explicit generic construction and alias/
callable-parameter precedence. Eleven isolated runtime/ownership cases pass,
including execution of the original static initializer on .NET.

The compiler portion of `scripts/test-baseline.sh` is now running separately.
Its project enumeration would otherwise also rebuild the Raven port during the
active frozen-host integration. Neither this running baseline nor the still
running semantic-result build is claimed as passed. A proposed bare generic-self
construction expectation was dropped: the tested C#-shaped translation retains
explicit type arguments (`Token<T>()`), without adding a Raven-specific rule.

While both validation runs remain active, an isolated symbol type-query draft
now emits against the accepted port-owned symbol interfaces. It translates the
named metadata-name forwarding overload, error-type traversal, nullable underlying
query and delegate Invoke lookup, plus the three new representation contracts.
The same eight C# tests pass against original C# helpers and the Raven draft,
covering recursion/cycles, nested errors, delegate signatures, null behavior and
contract ownership. This draft has not changed production port inputs. Full
symbol formatting and the ambiguity-reporting dependency remain unported.

A direct draft of metadata-identity helpers exposed a shared imported-nullable
gap. `ArrayTypeSymbol.FixedLength` imported from either the C# or Raven assembly
is represented as raw `System.Nullable<int>`: equality and conditional access fail,
while locally declared `int?` works. The reflection loader compares imported
nullable generic definitions by runtime Type object identity, which fails across
MetadataLoadContext. It also classifies the open runtime `Nullable<>` definition
as a nullable value. Two reduced C# boundary tests fail and the runtime constructed
nullable control passes. Production correction remains pending the active baseline;
the Raven translation retains nullable operators instead of working around the bug.

The imported-nullable correction now recognizes constructed core nullable types
within their reflection context, preserves the open generic definition, and
rejects user-defined namesakes. An isolated core build passes without replacing
the binaries used by either ongoing validation run. Eight of eleven new boundary,
semantic and runtime cases fail with the old compiler; all eleven pass with the
correction. All 77 adjacent nullable/target cases and 198 overload/generic cases
pass against the corrected assembly. neoCLR validation covers imported CLI
metadata semantics, including disabled source nullable-value declarations; runtime
execution is .NET only. The reflection loader has no Raven counterpart yet.

With this correction, the direct metadata-identity translation emits unchanged
nullable equality and conditional-access operations. The same fifteen C# tests
pass against C# and isolated Raven query/identity helpers, including fixed-array
lengths, assembly-independent nominal metadata identity and structural arguments.
These helpers remain outside production port inputs until the active integration
build completes. Full symbol formatting remains a later dependency slice.

The imported-nullable correction is committed as `8a7b133f1`. The constructor
lookup change remains staged independently while its compiler baseline runs.

The parameter-pattern metadata helper draft exposed two further compiler gaps.
Unparenthesized pattern tests followed by `&&`/`||` were rejected or consumed as
pattern operands. The C# parser correction passes 14 syntax/runtime regressions
(13 failed before), a 140-test baseline and 219 adjacent tests. Its Raven parser
counterpart waits for the active full build to finish so the validation inputs
remain fixed. No production Raven inputs have been changed during that build.

The faithful `GetSourcePattern` translation emitted invalid code when a conditional
chain invoked an extension on an intermediate struct property. Extension lowering
moves the receiver into argument zero; conditional emission must traverse that
argument's member/method chain. The correction passes six runtime cases, all
failing before, and 22 adjacent conditional-access cases. The draft's original
four invalid-program failures now pass unchanged. One remaining parity failure
concerns property-pattern trivia rewriting/normalization: C# yields
`{ Value: let x }`, while the accepted Raven syntax assembly retains
`{Value: let x}`. The parameter-pattern draft is not accepted or integrated yet.

Conditional extension receiver emission is committed as `8279ab260`. The full
semantic-result build completed in 44m12s. After correcting an ambiguous C# test
alias, parity ran 1,093 tests: 1,092 passed, and one metadata ownership test found
that `Conversion` getter-only backing fields lack `initonly`. The result-type
port remains uncommitted pending the compiler correction and rebuilt parity.

The newly built Raven assembly also resolves the older accepted assembly's
property-pattern update failure. Four direct update tests pass in both C# and
fresh Raven; three failed against the older Raven assembly. All 21 isolated
parameter-pattern helper tests now pass against the fresh assembly, without a
formatting workaround. These helpers remain a prepared dependency slice.

The constructor compiler baseline stopped on an obsolete expectation that a
local `Exception()` conflicts with imported `System.Exception`. The correct
lexical lookup eliminates that diagnostic. The test now expects successful
construction; rerunning it and completing the remaining baseline batches is
still required. The pattern logical-boundary correction has now been mirrored
into the Raven parser after the full build completed; rebuilt Raven validation
is pending.

Constructor lexical lookup is committed as `ed2d8a50b`. The completed compiler
baseline comprises 7,475 passing cases in the original 84 successful batches and
699 cases in the resumed seven batches, totaling 8,174. The obsolete shadowing
expectation was corrected before resumption; the failed batch was rerun and is
not double-counted. This validates the constructor snapshot, not later changes.

Readonly auto-property storage is committed as `c366e70ef`. Sixteen focused cases
pass (eleven failed before), alongside 87 adjacent semantic/metadata cases and
32 record/property execution cases. The corrected compiler also emits the four
translated result types with readonly storage; all sixteen isolated result tests
pass. neoCLR validation is semantic-only with CLI references; .NET execution
covers metadata and initialization behavior.

A new full integration build uses this corrected frozen compiler and reference.
It includes the result types, mirrored pattern logical-boundary correction,
symbol query/metadata identity helpers and parameter-pattern helpers. The existing
IParameterDefaultValueInfo port is reused. The shared C# suite passes 1,084 tests;
the expanded Raven suite is expected to contain 1,144, but has not yet run against
the rebuilt assembly. Sources and parity inputs remain fixed until validation
finishes. The prepared changes will be committed as separate dependency slices.


### Union content representation contract

Union content normalization now consumes `INullableTypeIdentity` instead of the
concrete nullable wrapper. Three independent-provider cases failed before the
correction; all eight focused cases pass afterwards. The 187-case union/display
baseline passed, and 190 adjacent union/display/C# union interop cases pass with
the correction. The isolated compiler build and whitespace checks pass. Validation
runs on .NET 11; no native neoCLR execution is claimed. The Raven helper remains
a draft pending integration and shared parity validation; carry this contract
check into that translation.


### Symbol formatter translation preparation

The complete remaining `SymbolExtensions` formatting implementation has an isolated
Raven draft, together with the display-only member of `AccessibilityUtilities` and
the complete union-content helper. The union helper emits against port-owned symbol
interfaces. The formatter and dependencies emit, and the same 21 C# contract cases
pass against the C# implementation and isolated Raven implementation: special-type
keywords, array rank/fixed length, pointers, escaped names, constructed generics,
namespace qualification, parameter ref kinds and constant defaults. These are
preparatory checks, not acceptance of the full formatter or integrated port.

The draft uses explicit suppression after existing null guards, preserving Raven's
documented static nullable-type semantics. Its immutable format local uses `??`
instead of reassigning a nullable parameter copy with `??=`. Core-only probe
configuration disables framework API projections, matching the port driver so
ordinary .NET out-parameter APIs remain available. The formatter's remaining
concrete Unit check has a draft `IUnitTypeIdentity` replacement; apply and validate
the corresponding C# refactoring before integration. More display branches, the
unit representation contract, complete-source review and shared integration parity
remain required. Production Raven inputs have not changed during the active full
helper build. No new compiler defect or native neoCLR execution is claimed here.


### Enum base classification discovered through formatter parity

The expanded formatter contract suite exposed an incorrect numeric accessibility
name for a private accessor: the C# result was `private init`, while the Raven
result contained a varying integer. Reduced enum instance-call tests failed seven
of eleven cases. Imported `System.Enum` was incorrectly classified as a value type
because its base is `System.ValueType`; concrete enum subclasses are value types,
but `System.Enum` itself is a reference-type class. Two of eight metadata-shape
cases failed across .NET and neoCLR's CLI-reference semantic paths.

An isolated override of `PENamedTypeSymbol.IsValueTypeLike` preserves reflection's
positive value-type/enum recognition and excludes `System.Enum` from the fallback
ValueType-base heuristic. The metadata correction alone passes all nineteen
focused cases and forty-eight adjacent enum, generic-call, struct-receiver and
display cases. A speculative emitter change was removed: it was not needed to
resolve the defect. Native imported symbols already take value/reference shape
from their metadata view rather than this CLI heuristic. No native execution is
claimed. Production integration of the override waits until the active full port
build releases its generator inputs. Metadata import is not yet Raven-ported;
carry this correction into that component.

Recompiling the complete formatter with the corrected isolated compiler restores
all thirty-six shared C# contract cases against both C# and Raven. The complete
DiagnosticBag draft now also emits with its generated reporters and both manual
overloads. Fifteen tests pass, including all 366 generated reporters with null
and non-null arguments, severity behavior, bag behavior/concurrency/ownership,
and four ambiguity-candidate cases. The same four ambiguity cases pass against
C#. Candidate formatting uses the full real formatter, filtering null candidates
and retaining the first two. These remain isolated drafts pending production
integration, source review, and full shared parity.


### Formatter grouping and final draft review

Formatter coverage now includes framework delegates, named and implicit tuple
elements, structural unions, intersection grouping, and return-type projections.
A new neoCLR-owned contract case exposed a shared display defect: a structural
union inside an intersection printed `A | B & C` instead of `(A | B) & C`. Both
implementations failed that case before correction. Isolated C# and Raven drafts
now group structural unions as well as the standard union carrier; all 48 shared
contract cases pass in each implementation, and 55 existing C# display cases pass.
The C# probe also includes the prepared Unit representation-contract refactoring.
Neither C# formatter edit has been applied to live generator inputs yet.

The Raven formatter's lambda blocks and immutable locals have been reviewed and
formatted. Public top-level accessibility is retained explicitly across partial
declarations; the public-by-default member convention does not justify changing
the exported type's accessibility. The internal diagnostic-scope helper remains
open to preserve its C# inheritance shape. The combined diagnostic-reporting
draft, rebuilt with the corrected enum-import host, still passes all 15 tests.
A project patch is prepared to generate both the diagnostic catalog and Raven
reporters from the maintained generator. The active full helper-build manifest
was checked: all 1,190 recorded inputs remain unchanged. Integration and separate
validated commits remain pending that build's completion.


### Enum base classification stabilization

Formatter parity exposed corrupted enum instance receivers: imported `System.Enum`
was treated as a value type because it derives from `System.ValueType`. Preserve
positive runtime value-type recognition, but classify the abstract `System.Enum`
base as a reference type. Concrete enums retain value semantics. Metadata import
is not yet ported; carry this correction into its Raven translation. Native
symbols already obtain this distinction from their metadata view.

Validation: 19 focused metadata/runtime cases and 48 adjacent cases pass; the
required `scripts/codex-build.sh` succeeds. Metadata tests cover .NET and neoCLR
CLI-reference semantics; runtime tests execute on modern .NET only. No native
neoCLR, .NET Framework or NanoFramework execution is claimed.


### Imported factories and cold semantic queries

The full helper integration build failed before parity ran: imported syntax
factory calls such as `Token(...)` selected a same-named parser type after the
lexical constructor lookup change. A local type now retains precedence over
imported types while allowing imported static value members to remain callable.
Type annotations still resolve the local type. Available invocation queries probe
functions before constructor fallback without forcing declaration completion.
Source types retain their original symbols instead of passing through metadata
lookup. Existing caching tests caught that unnecessary completion during review.

The focused collision cases exercise cold and diagnostics-first queries on both
.NET and neoCLR CLI-reference semantic paths, plus execution on modern .NET.
Native execution is not claimed. Binding and semantic-model implementation are
not yet Raven-ported; preserve these corrections in their future translations.
The full Raven helper build and shared parity still require a new compiler host;
the failed build is not evidence of completed port integration.

Validation: all 170 focused and adjacent import, constructor, alias, semantic
cache and incremental-binder cases pass. The targeted compiler build succeeds.


### Formatter grouping and representation contracts

Shared display contract tests reproduced ambiguous formatting for a structural
union inside an intersection (`A | B & C`). Both formatter implementations now
retain `(A | B) & C`. Function parameter formatting uses `IUnitTypeIdentity`
instead of a concrete bootstrap unit-symbol type. The Raven formatter has been
translated into the port worktree; its full integration remains pending.

Validation: the new 59-case display/diagnostic baseline failed only the grouping
case. After correction, 119 focused and adjacent C# cases pass, and all 1,151
shared tests pass against the C# implementation. Earlier isolated Raven formatter
coverage passed 48 cases; this is not a substitute for the full port build.
Intersection display tests describe neoCLR semantic shapes, not native execution.

### Open-class/interface compatibility discovered by symbol-provider port

The direct `SymbolBase` translation exposed rejection of interface type patterns
against an abstract class that does not itself implement the interface. Shared
conversion classification now permits open-class/interface explicit reference
conversions in either direction; type patterns admit these conversions without
user-defined conversions. Closed classes still require an implementation.

Validation: production C# compiler build succeeded; 11 focused cases passed,
including .NET and neoCLR binding with host references and .NET runtime casts and
matches. A production-core adjacent invocation of the match/conversion/interface
filters passed 345 cases. Earlier isolated baseline and post-change filters each
passed 260 cases. Native neoCLR execution was not tested. The binder/conversion
implementation has not yet been ported; preserve this rule when translating it.

The separate full Raven formatter/helper build remains in progress using its
frozen host. This compiler fix does not change that build's inputs. The isolated
symbol-provider port also exposed missing caller information and external enum
constant resolution; those fixes remain separate pending slices.

### External enum default resolution discovered by symbol-provider port

The direct `SymbolBase` translation imports an enum for an optional constructor
parameter. Final PE writing failed when Cecil tried to resolve that enum by file
name rather than through the supplied reference. The final writer now supplies a
resolver keyed by explicit reference assembly identity, retaining base resolver
fallback and disposing loaded definitions after writing. This applies to normal
and retargeted CLI emission. Code generation has not yet been translated to Raven;
the future port must preserve this reference-resolution behavior.

Both focused regressions failed before the fix and pass against the production
compiler afterward. They inspect the enum parameter type/default and execute an
omitted-argument call. The production build and 29 adjacent normalization, enum
receiver and enum classification tests pass. Validation used .NET 11 only; no
.NET Framework, NanoFramework or native neoCLR execution is claimed. Runtime
Contract and CLI bridge documentation record ownership and the native replacement.

### Named and generic collector arguments — caller-info stabilization

Caller-expression testing exposed rejection of a named scalar for a collector
parameter. Shared overload mapping now lets normal/expanded conversion rules
select the collector or its element, retaining duplicate-name rejection. Follow-up
regressions exposed generic single-element inference stopping after an inference
walk that produced no usable collection conversion. Normal-form bounds are now
provisional; expanded inference retains the original bounds when normal form is
inapplicable. Array/nullable substitution and collection-expression pre-inference
preserve target typing without confusing a collection value with its elements.

Validation: the complete 25-case regression fixture had 12 failures before this
slice and passes afterward. It covers source and imported collectors, named and
positional scalars, generic arrays, collection expressions, a collection passed as
one element, nullable collectors, overload selection and invalid arguments. Three
cases check neoCLR binding with host references; the rest use .NET execution or
C# behavior/diagnostics. Native execution is not claimed. The production build and
all 498 adjacent overload-suite cases pass. These binder/resolver components have
not been translated yet; preserve the corrections in their future Raven ports.

The full formatter/helper Raven build separately completed successfully. Its
1215-case shared suite has one remaining tuple-pattern trivia-rewrite/display
failure (1214 passed), so acceptance of those pending port slices remains open.

### Accepted port slice: semantic result values

Ported `Conversion`, `SymbolInfo`, `CandidateReason` and `TypeInfo` beside their C#
implementations. `Conversion` retains explicit symbol-aware equality and hashing;
`SymbolInfo` remains a readonly record struct; `TypeInfo` remains an inheritable
record class. The translation uses port-owned symbol signatures and preserves
constructor/default behavior, readonly metadata, record copying and derived-record
equality. C# remains the maintained bootstrap implementation.

Validation against the completed full Raven assembly: all 13 shared C# value
contracts and all 15 Raven value/ownership contracts pass on .NET 11. The full port
build succeeds. Its one unrelated tuple-pattern rewriting failure remains tracked
separately (1214 of 1215 full parity cases pass); this commit does not claim that
the entire integration gate is green. No native target execution is claimed.

### Stabilization: preserve constructed storage type identity

The full Raven syntax-tree parity failure reduced to a three-file declaration
ordering defect: `List<Element>` resolved successfully, but early constraint
validation replaced the property type with `Error` before `Element`'s base type
was bound. Subsequent binding retained that poisoned property symbol, and a
valid generated `Update` condition silently became an error expression.

Storage constraint diagnostics now leave the resolved constructed type intact,
as C# does even for genuinely invalid type arguments. Constraint errors remain
reported. The unported shared C# binder supplies the correction to both target
binding paths and to the Raven bootstrap; no generated Raven workaround is used.

Validation: production compiler build; 26 new symbol/oracle cases and 56 adjacent
generic/constraint cases passed. Six .NET execution cases passed (two failed
before). The 1,189-source Raven probe now binds the original generated condition
as Boolean with the expected list inequality operator. neoCLR tests cover binding
with host references, not native execution. Full Raven rebuild/parity remains a
separate pending gate before accepting the remaining port slices.

### Imported nullable operator references

The port's imported `SyntaxToken` equality calls exposed a CLI metadata defect:
`Nullable<SyntaxToken>` inherited the payload assembly scope. Correcting that
scope alone was insufficient because the normalizer ignored newly constructed
parameter signatures and could remove their still-needed assembly reference.

The shared C# emitter now assigns the core-library wrapper scope and traverses
replacement member signatures during scope normalization and reference retention.
The emitter is not yet Raven-owned; the direct Raven port needs no workaround.
All 76 operator and adjacent metadata/runtime cases pass, including the two
previously failing imported operator cases. Modern .NET was executed; native
neoCLR was not. The concurrent full Raven rebuild retains its frozen bootstrap
and is a separate integration gate.

### Accepted parser boundary slice

Both parser implementations now leave `&&` and `||` outside an `is` pattern.
Shared C# syntax and generated-pattern update tests run against both compilers;
separate .NET execution tests cover short-circuit behavior. The grammar-level
contract is recorded in the pattern specification and syntax API documentation.

Validation: the complete Raven build succeeded, with 1,215 Raven parity cases
and 1,151 C# parity cases passing. This establishes modern .NET behavior;
native neoCLR execution is not claimed.

### Accepted symbol query and identity slice

The Raven compiler now owns the type-query and metadata-identity helpers and
read-only representation contracts used by symbol formatting. The translation
preserves fixed-array lengths, structural arguments and the distinction between
symbol equality and nominal metadata identity. Shared C# contract tests run
against both implementations.

Validation: the complete Raven build succeeded, with 1,215 Raven parity cases
and 1,151 C# parity cases passing. This establishes modern .NET behavior;
native neoCLR execution is not claimed.

### Accepted parameter pattern metadata slice

`ParameterPatternFacts` and the `.None` optional-parameter marker now have Raven
implementations. Shared C# tests cover metadata encoding/decoding, source patterns
and normalized display text. The port uses the generated syntax API directly;
it does not compensate for the earlier generated `Update` binding defect.

Validation: the complete Raven build succeeded, with 1,215 Raven parity cases
and 1,151 C# parity cases passing. This establishes modern .NET behavior;
native neoCLR execution is not claimed.

### Accepted symbol display slice

Symbol formatting, accessibility display and union-content nullability helpers
are translated to Raven. Shared C# tests cover display formats, structural-type
grouping and independent symbol providers. The implementation preserves the C#
behavior and keeps target-specific runtime representation separate from shared
language-level display.

Validation: the complete Raven build succeeded, with 1,215 Raven parity cases
and 1,151 C# parity cases passing. This establishes modern .NET behavior;
native neoCLR execution is not claimed.

### Accepted diagnostic reporting slice

Raven now owns `DiagnosticBag`, its disposable reporting scope, both handwritten
reporting helpers and all 366 generated descriptor-based reporters. The project
runs the preserved DiagnosticsGenerator Raven modes alongside catalog generation.
Shared C# behavior/concurrency tests and Raven ownership checks verify the port;
differential tests check every generated reporter's diagnostic and argument order.

Validation: the complete Raven build succeeded, with 1,215 Raven parity cases
and 1,151 C# parity cases passing. This establishes modern .NET behavior;
native neoCLR execution is not claimed.

### Current integration acceptance

The five port slices above are accepted together after the full Raven compiler
build completed in 41 minutes 54 seconds. The frozen C# bootstrap contains the
storage-type identity correction (`8351c6805`); its inputs remained unchanged
through the build and automatic parity run. All 1,215 Raven parity cases passed,
including the previously failing normalized positional-pattern display case.
The current production C# compiler also passed all 1,151 shared parity cases.
Both suites passed again after whitespace formatting of the touched C# files.

Earlier running/pending statements in this progress log describe historical
validation attempts, not the current status of these accepted slices. The full
compiler port remains incomplete. Parameter-identity, caller-information and
initializer semantic-query fixes are still isolated drafts awaiting broader
baseline validation and separate integration. The C# bootstrap remains beside
the Raven implementation, and .NET remains the default target.

Accepted commits:
- `ab4244891`: pattern logical boundaries.
- `10ca76e27`: symbol query and metadata helpers.
- `8ad72d367`: parameter pattern metadata.
- `e8d3ff59e`: symbol display.
- `8a4f25beb`: diagnostic bag and reporters.

### Imported generic signature stabilization

The suppression-map port exposed an emission defect while calling imported
methods with generic tuple results. The broad baseline also reproduced it in
`RetargetedNestedTupleFieldsPreserveUnderlyingGenericIdentity`. Scope rewriting
now visits the named types in a signature and preserves each generic parameter’s
declaring owner. Imported method references retain CLI primitive encodings inside
arrays and generic arguments. Three C# execution tests cover nested value tuples,
generic tuple returns and generic methods returning primitive arrays.

### Diagnostic ordering

The Raven port now owns `DiagnosticComparer`. It preserves C# ordering by
location, identifier, severity, message, suppression state and diagnostic
properties. The same 19 C# tests exercise both implementations, including stable
ordering across property enumeration orders and case-insensitive path comparison.

The integrated suites passed 1,170 C# parity cases and 1,234 Raven parity cases
on modern .NET. The Raven build used the frozen bootstrap containing the assembly
scope correction in `7a11008d8`.

### Emission results

The Raven `EmitResult` carries the backend's success flag and its diagnostics.
The constructor normalizes a default diagnostic array to an empty array and
retains a supplied array. Both parity projects run the same C# contract tests
and verify that the selected compiler assembly owns the result type.

### Source diagnostic suppression

The C# and Raven suppression maps process directives in source order.
`disable-next-line` applies to its effective line while earlier directives
continue to govern subsequent lines. Both implementations run the same C# tests
for overlapping IDs, wildcard suppression, restores and protected diagnostics.
A compilation-level test checks both normal and suppressed diagnostic reporting.

### .NET metadata input snapshots

The Raven implementation now captures the ordered file stamps and import options
used to decide whether a .NET metadata session can be reused. Shared C# tests
exercise relative paths, duplicate inputs, missing files, changes to length and
write time, and equivalent or changed import options.

### .NET metadata reference resolution

The Raven port owns the reference set and its stream-backed metadata resolver.
It keeps the first supplied path for each full assembly identity. Resolution
prefers exact identity and then uses simple-name lookup in sorted path order.
Shared C# tests exercise duplicate identities, missing and corrupt inputs, PE
identity decoding and the image callback used when loading an assembly.

### .NET interface metadata decoder

The Raven decoder reads direct interface declarations from captured assembly
images. Shared C# tests check generic and nested interface types, the positions
of nullable flags, and repeated reads in the same metadata context.

### .NET metadata session

The translated session retains its context and captured interface images across
compatible input snapshots. Shared tests cover loading by path and identity,
fallback after a missing or malformed path, and decoding interfaces after the
original file has been deleted.

## Declaration keys and table

The C# and Raven implementations index syntax declarations in a single traversal.
Each key records the declaration's generic arity and containing declaration.
Constructing a new table enumerates its input trees once and reuses key instances
for retained trees. Shared C# tests exercise both implementations, including
record equality, detached syntax, discards and incremental reuse.
