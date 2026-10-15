# Structural type lowering for neoCLR

This describes the development branches `codex/neoclr-union-intersection` in Raven
and `codex/structural-union-intersection` in neoCLR, as inspected on 2026-10-15.
It is a compiler-to-runtime mapping, not a claim that every form is qualified for
JIT/AOT or available on main. See [the bridge record](neoclr-cli-bridge.md) for
consumer evidence and [Runtime Contracts](runtime-contracts.md) for target setup.

## Source types and metadata

| Raven type | Compiler representation | Native metadata |
| --- | --- | --- |
| `A \| B` | `IStructuralUnionTypeSymbol`, constituent types | Union signature containing the constituent signatures |
| `A & B` | `IIntersectionTypeSymbol`, constituent types | Intersection signature containing the bound signatures |
| Named tuple elements | `ITupleTypeSymbol`, ordered `TupleElements` | Tuple signature with ordered element types and names |
| Nominal generic containing a composition | Nominal constructed type with a structural argument | Generic instantiation whose argument is the structural signature |

A structural union or tuple has value semantics without a nominal base class.
Metadata describes its type directly; Raven does not emit a new nominal type
for each combination. Runtime storage is a separate decision. Tuple names survive
native metadata and reflection but do not affect positional type compatibility.
Native tuple symbols have no `UnderlyingTupleType`; their elements are not carrier
generic arguments. Named union declarations remain nominal and keep their case
contracts. An error alternative such as HttpError can itself be a nominal union.

`MetadataImportOptions.WithStructuralCompositions()` selects native structural
binding; `WithStructuralTuples()` selects native tuples. Emission also checks the
matching `NeoClrEmitOptions` capabilities, including `AllowExperimentalCompositionCalls`,
`AllowExperimentalIntersectionCalls` and `AllowExperimentalTuples`. These are
explicit target contracts, not automatic changes to ordinary .NET compilation.

`Int32Emitter.ImportExternalType` creates composition signatures with
`CreateExperimentalReferenceComposition` and tuple signatures with
`CreateExperimentalTuple`. The historical “Reference” name of that host API does
not make the union a reference type. `NeoClrTypeMapper` recursively maps generic
arguments. Imported structural signatures are resolved through native metadata
rather than reconstructed from a nominal System.Union declaration.

For `Task<Response<T> | HttpError | JsonError>`, Task and Response retain nominal
identity, while Task's argument is a union signature. T retains its owner/method
scope inside Response. This does not imply support for a bare `T | Error` or every
open constraint. Those remain separate qualification boundaries.

## Patterns over structural unions

A type pattern tests the **active payload's runtime type**, not a numbered case
on a generated union carrier. Reference alternatives use nominal assignability:
implementing an interface must be explicit. Several patterns can match the same
object when alternatives overlap, so arm order and guards matter. The union's
normalized alternative order is not the match-arm order.

The shared match lowerer evaluates the subject into a temporary, then considers
arms in source order. Pattern tests bind variables on success; the guard runs
after binding. A failed test or false guard proceeds to the next arm. A match
expression converts the selected arm result to its result type, stores it in a
result temporary and branches to the join. Abrupt arms retain their control flow.
The compiler's exhaustiveness analysis is separate from these runtime tests; a
runtime fallback remains in the shared lowering.

For a reference declaration pattern with a binding, `PatternValue` emits this
sequence (schematic native instructions, not a standalone Raven sample):

```text
stloc subject
ldloc subject
isinst Response
ref.isnull
ldc.bool false
ceq
brfalse next_arm
ldloc subject
castclass Response
stloc response_binding
... guard, then arm body ...
```

`TypeTest` is the `isinst`/null-test/false-equality sequence. It produces a Boolean;
a failed pattern does not perform the subsequent checked extraction. A successful
reference extraction preserves the payload object's identity. A cast written by
the developer without a preceding successful test may fail instead of selecting
another arm.

A value declaration pattern uses the same test, followed by exact unboxing:

```text
ldloc subject
isinst Int32
ref.isnull
ldc.bool false
ceq
brfalse next_arm
ldloc subject
unbox.any Int32
stloc int_binding
```

This distinguishes Int32 from Int64 even if their numerical values are equal.
A reference property pattern first tests and casts its receiver, then invokes its
property getters and recursively tests those values. A null constant uses
`ref.isnull`; discard consumes the subject without extraction. Negation without
bindings reverses the success/failure branch. Other patterns still depend on the
portable backend's explicit admission rules; support must not be inferred merely
because the frontend accepts a pattern.

Nominal union case patterns take a different path: the bound case/member pattern
supplies a `TryGetMethod`, which the portable lowerer calls before binding or
recursively matching its payload. Structural reference/value declaration patterns
above do not use those helpers. Matching a nominal case inside a structural
alternative therefore has both an outer type-selection problem and the nominal
case contract; qualification of one does not establish every nested combination.

### Pattern qualification boundary

Reference-only native unions have focused AOT/interpreter tests for membership,
checked extraction and interface conformance. Scalar-only native unions now lower
`isinst Int32`, `isinst Int64` and `isinst Boolean` followed immediately by
`ref.isnull` using their inline discriminator. The result is an internal test token,
not an allocated Object view. Nominal Object queries retain their boxed-query path.

The runtime instruction sequence emitted above is checked against the interpreter
for each active alternative and default null, including false Boolean payloads,
exact extraction and mismatches. A [fresh Raven-produced consumer](https://github.com/marinasundstrom/neoCLR/tree/codex/structural-union-intersection/docs/experiments/native-structural-types/scalar-pattern-project)
also passes six guarded/fallback-arm checks and a match-result join in the
interpreter and macOS ARM64 AOT, with no allocator imports or arena use. It uses
the recorded v3 library bundle. An artifact-only library consumer also passes
union returns and parameters with reordered source alternatives, with the same
zero-arena result. Broader application/JIT/collector coverage still needs qualification. A test subject must currently be
an immediate union local/argument load or union cast. Escaping or duplicating the
`isinst` result and compound-union test targets remain gated. Mixed scalar/nominal
reference unions now have bounded runtime AOT tests using separate numeric and
traced reference slots. A fresh Raven `int | long | bool | Response` consumer also
passes seven scalar/reference pattern inputs in interpreter and AOT, using 16
arena bytes only for the explicit Response allocation. An artifact-only imported mixed consumer also passes returns, reordered union
parameters and imported Response property access with the same allocation.
An imported Response<int> variant also preserves its stored generic field value
through those calls, with a distinct reference-arm result and the same 16-byte
object allocation. Bare generic alternatives and broader application coverage
remain open.
Object views requiring scalar boxing, intrinsic String and nominal-value
alternatives remain gated.
Do not infer complete Raven match support from these bounded runtime tests.

## Conversions and physical storage

| Operation | Raven portable instruction | Native operation |
| --- | --- | --- |
| Reference conversion | `ReferenceConvert` | `castclass` with the native target signature |
| Value admission to a union | `BoxToObject`, then `ReferenceConvert` | `box T`, then `castclass union<...>` |
| Value extraction | `UnboxAny` | `unbox.any T` |
| Runtime type test | `TypeTest` | `isinst T`, null test, Boolean inversion |
| Tuple construction | `NewTuple` | Native tuple construction |
| Tuple element read/write | `LoadTupleElement` / `StoreTupleElement` | Typed positional element operations |

The interpreter currently uses boxed value payloads for the source conversion
sequence. Native reference-only unions copy one payload-reference slot without a
carrier. The scalar-only backend work uses a tag plus numeric payload and removes
an exact adjacent box/cast allocation for Int32, Int64 and Boolean. Ordinary
Object boxing is unchanged. These representations do not define a public ABI.
Bounded mixed scalar/nominal-reference unions now add a separate reference slot
alongside the tag and numeric payload; scalar injection clears it. Only that slot
is traced. This spends an extra slot to keep numeric bits out of reference tracing.
Closed nominal value-record unions now have bounded native tests using disjoint
inline payload regions and an exact nominal discriminator. The adjacent box/cast
pair copies fields and clears inactive regions; unbox.any checks identity before
extracting a snapshot. Embedded references keep their identity and tracing.
This sums alternative sizes (up to 256 lanes), trading space/copy cost for
unambiguous reference tracing. A fresh artifact-only Raven struct consumer now passes seven union-call/pattern
cases, including preserved Int32/Int64 fields and reordered alternatives, with
no native imports or arena allocation. Native backend tests also admit records mixed with direct nominal reference
alternatives. One reference region stores the admitted object; reference tests
and casts inspect actual nominal conformance, including overlapping interfaces.
Inactive regions stay zero. A fresh artifact-only Raven consumer also passes eight scalar/record/reference
inputs through imported reordered signatures and typed patterns. It preserves
Response struct fields and HttpError class properties with only the explicit
HttpError allocation (16 bytes). It uses the retained v3 bundle; whole-union
boxing, managed JIT/GC and throughput qualification remain open.
A fresh nine-input imported consumer additionally combines Response<int> and
Response<long> value records with scalars and HttpError. Their two-field layouts
remain distinct constructed types; guards read correctly substituted fields.
Interpreter/AOT agree (42), with only the explicit HttpError allocation (16 bytes).
This required fixing general pattern reachability to retain complete type identity,
selectively carried from main-based commit f7a4bc5a4. It does not qualify all open
generic execution. A further ten-input consumer qualifies Response<Cell> beside
the numeric records and HttpError: copying the value record preserves shared
mutation of the embedded Cell. Interpreter/AOT agree with exactly the two explicit
object allocations (32 bytes). The arena host does not qualify collector behavior.

Intersections are views satisfying every nominal bound; they introduce no trait
implementation or generated members. Native bounded views occupy one reference
slot. Classes closed to extension in the .NET sense are rejected as bounds;
Raven sealed hierarchies are a separate concept. Constraints retain structural
signatures in native metadata; they are not flattened into invented nominal types.

Native tuples use ordered inline storage, including nested copies and element
addresses. Their element names are metadata, not TupleElementNames attributes.
Arrays and function types have their own mappings and admission limits; neither
is implemented by inventing a structural union carrier.

See also neoCLR’s [structural runtime operations](https://github.com/marinasundstrom/neoCLR/blob/codex/structural-union-intersection/docs/structural-runtime-operations.md) for callable binding/invocation and the runtime instruction contracts.

## Implementation and evidence map

- [Match lowering](../../src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.MatchExpression.cs): subject/result temporaries, guards, arm ordering and joins.
- [Portable lowering](../../src/Raven.CodeAnalysis/CodeGen/Portable/LinearMethodBody.cs): `PatternValue`, `UsesReferenceStorage`, conversions and tuple operations. `UsesReferenceStorage` describes today's operation path, not semantic reference-type identity.
- [Native instruction emission](../../src/Raven.CodeAnalysis.NeoClr/NeoClrLinearMethodBuilder.cs): exact `TypeTest`, boxing, casting and tuple mappings.
- [Native signature emission](../../src/Raven.CodeAnalysis.NeoClr/Int32Emitter.cs) and [type mapper](../../src/Raven.CodeAnalysis.NeoClr/NeoClrTypeMapper.cs): native composition signatures and generic nesting.
- [Structural union symbol tests](../../test/Raven.CodeAnalysis.Tests/Symbols/SourceStructuralUnionTests.cs) and [tuple binding tests](../../test/Raven.CodeAnalysis.Tests/Semantics/Functions/NeoClrTupleTypeTests.cs): frontend contracts, not full native execution evidence.
- [neoCLR native qualification](https://github.com/marinasundstrom/neoCLR/tree/codex/structural-union-intersection/docs/experiments/native-structural-types): runtime storage, AOT/JIT scope, reproducible tests and tuple measurements.

Ordinary .NET emission and the legacy CLI bridge retain their nominal encodings.
Do not infer their representation from this native document. Keep this map current
when a compiler instruction, metadata signature, pattern path or runtime storage
strategy changes; record validation separately from intended behavior.


### Native collection qualification

The neoCLR backend now has bounded AOT collector stress for unions containing
records with references, direct references and scalars. Published arguments,
locals and call operands/results preserve the live object through eighty temporary
array allocations in a small heap; normal/fault unwind removes the root chain and
permits complete reclamation. Both eager and paced nonmoving collection are tested
against interpreter results with native sanitizers. This changes no Raven emission,
metadata or Runtime Contract setting. Fresh imported Raven consumer evidence is
still arena-only; JIT, async retention and broader structural shapes remain open.
