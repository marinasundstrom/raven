# Opt-in neoCLR extended CLI output

Development slice, 2026-11-14. The neoCLR target can explicitly select the bounded
CLI-authoritative metadata writer. Ordinary .NET emission and default neoCLR native
output remain unchanged. This is a persistence selection, not new language semantics
or a Runtime Contract binding change. The Raven porting branch is not involved.

## Driver

```sh
dotnet rvnc.dll neoclr --extended-cli --library \
  --native-core-reference /path/to/System.Runtime.dll \
  --reference /path/to/tuple-library.dll \
  -o /fresh/output/TupleConsumer.dll Forward.rvn
```

`Forward.rvn` can forward a nested structural tuple into an extended dependency:

```raven
import Example.Values.*

public func Forward(value: ((int, int), int)) -> ((int, int), int) {
    return Echo(value)
}
```

The neoCLR repository's tested `samples/metadata-forwarding` also forwards
`int | bool`. The direct compiler driver accepts `--extended-cli` once. This first
slice does not add a `.rvnproj` property or alter project defaults. Failed emission
reports NEOMETA003 and creates no output. Unsupported CLI options still fail during
argument validation. Supply dependencies explicitly when executing the result.

## Host API

`Raven.CodeAnalysis.NeoClr` adds:

```csharp
public enum NeoClrOutputFormat { Native, ExtendedCli }
// Added final optional argument to NeoClrEmitOptions constructor:
// NeoClrOutputFormat outputFormat = NeoClrOutputFormat.Native
public NeoClrOutputFormat OutputFormat { get; }
```

Create immutable options with `outputFormat: NeoClrOutputFormat.ExtendedCli`, then
pass them to `NeoClrEmissionBackend` through `EmitOptions.WithBackend`. Undefined
enum values throw `ArgumentOutOfRangeException`. Rebuild precompiled C# hosts for
the extended constructor signature; existing source calls retain their defaults.

`Native` retains the existing `emitMetadataAssembly` distinction between binary
PE/#Neo and JSON interchange. `ExtendedCli` selects its own authoritative PE writer
regardless of that legacy transport flag. It does not change target capability
checks, symbols, tuple/union identity, nullability semantics or ownership bindings.
No language-service or grammar change is required for this output choice.

## Encoding and bounded support

Ordinary MethodDef/Param/AssemblyRef/MemberRef rows and executable CIL are
persisted. Parameter names use standard Param rows and retain their source positions.
Structural signatures and namespace-scoped imports use neoCLR's versioned #Nex
extension. There is no duplicated #Neo definition payload or synthetic tuple carrier.
This output targets neoCLR, not an unmodified .NET CLR.

The writer admits global nongeneric forwarding functions, the previously qualified
Int32/Boolean/tuple/scalar-union signatures and empty local interface declarations.
Raven may bind a larger source program than this bounded writer can persist;
unsupported declarations/bodies fail rather than losing metadata or falling back.
The native writer remains the default. Legacy tuple/composition imports reject
extended dependencies; that guard is not universal across symbol-authored imports.
Mixed-format array dependencies are not qualified by this slice.

Explicit Raven namespace/module declarations create a logical-module manifest.
The matching metadata writer now preserves it (see the logical-module follow-up
below), so forwarding functions may be declared in an explicit namespace/module. Nullable
annotations, parameter defaults/modes/attributes, general instructions,
external interface references and broader source-library output remain gaps.
This slice does not qualify the original executable probe that creates a tuple;
it qualifies Raven-produced forwarding libraries executed by a runtime host.

## Validation

`tools/NeoClrMetadataProbe --extended-cli <core> <tuple-library> <Forward.rvn> <output>`
checks import/parameter metadata, both backend transport arguments, native defaults,
unsupported arithmetic diagnostics, explicit logical-module preservation and atomic failure.
The focused NeoClrProfileTests pass (21 tests, including .NET/neoCLR profile cases).

Companion neoCLR checks execute the CLI-driver-produced tuple and both union
alternatives in interpreter and native arm64 execution (84/42), validate sparse Param
names/ownership and malformed rows, and retain prior structural regression coverage.
Use matching Param-capable neoCLR metadata/runtime binaries. The companion evidence
is `docs/validation/raven-extended-cli-20261114/README.md` in neoCLR, with exact fixture
and compiler hashes. Compiler integration lives on unified main; no port-branch merge
or complete extended-CLI application/class-library migration is claimed.

## Vector-array qualification (2026-11-14)

With the matching neoCLR metadata API, the unchanged Raven emitter can persist
`int[]` forwarding functions through `--extended-cli`. The neoCLR sample
`samples/metadata-forwarding/ForwardArray.rvn` imports `Example.Arrays.Echo`; its
MethodDef/MemberRef signatures use standard SZARRAY and retain parameter names.
Both neoCLR engines observe mutation through the original array after forwarding.
The supplier also uses array TypeSpec tokens. No source semantics, Runtime Contract
configuration, .NET target behavior or compiler implementation changes are needed.

Raven's callable-import capability still rejects `EchoJagged(int[][])` with
NEOMETA001, although the metadata API and runtime can persist/execute jagged arrays.
This is a provisional compiler profile gap, not a language or runtime restriction.
The checked negative source and exact compiler/fixture hashes are in neoCLR's
`docs/validation/extended-cli-arrays-20261114/`. Ordinary native output remains the
default; no porting-branch merge or array-view redesign is included.

## Jagged callable imports (2026-11-14 follow-up)

The neoCLR emitter now recursively admits vector elements in imported callable
signatures. `int[][]` remains an array of arrays, represented by nested SZARRAY
nodes in extended CLI and existing recursive ArrayRef nodes in native metadata.
The previous explicit nested-array exclusion in the target-owned symbol contract
check is removed; rank-one and non-fixed-length checks apply at every level.
The existing recursive mapper supplies the same element types without reopening
metadata definitions or introducing nominal carriers. Use the matching metadata
API, whose CreateFunctionReference validation now admits the same recursive vectors.

The earlier NEOMETA001 jagged probe records the prior limitation. It is now covered
by `tools/NeoClrMetadataProbe --extended-array-imports <core> <array-library>
<ForwardArray.rvn> <output>`, which validates vector/jagged signature shape, parameter
names, import namespaces and unchanged native output selection for a local jagged
identity function.
The direct compiler output executes in both neoCLR engines; replacing a returned
jagged-array slot is visible through the original outer reference. Evidence lives
in neoCLR's `docs/validation/raven-jagged-imports-20261114/`.

No Runtime Contract configuration, syntax, semantic model, .NET backend or language
service behavior changes. This fixes neoCLR import admission only; multidimensional
and fixed arrays remain outside this import path, and this slice does not qualify
all possible nested element types or general array construction in extended output.

## Explicit logical modules (2026-11-14 follow-up)

The matching neoCLR metadata writer now persists explicit namespace/module
manifests with required #Nex bit 0x10 and #Strings name indexes. The existing Raven
source emitter already calls DefineModule; its language/semantic/import behavior
is unchanged. A physical CLI Module row still describes the image, not these
logical declaration containers. Empty declared modules are retained; no synthetic
TypeDefs or parent names are introduced. Old extended readers reject the new bit.

The updated `--extended-cli` metadata probe checks explicit `Declared` ownership
instead of expecting its previous rejection. neoCLR's `ForwardModule.rvn` sample
contains a namespaced nested-tuple forwarder and an empty sibling declaration.
Actual direct-driver output passes in the interpreter and native arm64 engine,
returning 84; manifests retain both declared names. Evidence and hashes are in
neoCLR's `docs/validation/extended-cli-modules-20261114/`.

No Runtime Contract setting, parser, semantic model, language service, .NET target
or default persistence selection changes. General source bodies/constants/classes
remain outside the extended writer profile; this is not complete library migration.

## Typed locals (2026-11-14 follow-up)

The matching metadata writer now admits existing Raven local declarations and
loads/stores through standard StandAloneSig/LOCAL_SIG and CIL ldloc/stloc. The
compiler implementation, type inference, definite-assignment rules, semantic model
and Runtime Contract settings are unchanged. Ordinary .NET and default native
output selection are unchanged.

neoCLR's `samples/metadata-forwarding/ForwardLocals.rvn` stores a tuple returned by
an imported function, reassigns it through a second call, and returns it; another
function stores/returns Int32. Actual direct-driver output contains the typed local
signatures and executes in both engines (84/42). The previous writer rejected the
same source with NEOMETA003. Evidence/hashes are in neoCLR's
`docs/validation/extended-cli-locals-20261114/`.

The extended writer emits InitLocals unset and validates store-before-load through
its existing graph checker; it does not introduce default structural values. The
limit is 256 locals/body. Local names/debug scopes, branches, arithmetic, explicit
initialization, address-taking, exceptions and general executable output remain
outside this bounded qualification. Old C# declaration readers reject the newly
admitted standard table; no #Nex bit or runtime type redesign is introduced.


### Extended CLI Int32 arithmetic qualification (2026-11-14)

The matching neoCLR metadata writer now persists existing Int32 add, subtract,
multiply and negate operations as standard CIL 0x58/0x59/0x5A/0x65. No compiler
binding or lowering change is needed: parameterized Raven arithmetic preserves
operand order and overflow wrapping, including minimum-integer negation, in both
neoCLR engines. Runtime Contract settings, native output defaults and the ordinary
.NET target remain unchanged. The unsupported-body regression now uses division
and still requires atomic NEOMETA003 failure; native division emission succeeds.

Reproduce with direct `rvnc neoclr --extended-cli --library`, selecting the native
core using `--native-core-reference`, and neoCLR's
`samples/metadata-forwarding/Arithmetic.rvn`. Source/artifact hashes and commands
are recorded in `docs/validation/extended-cli-arithmetic-20261114/` in neoCLR.
This qualifies the native semantic model's standard CLI persistence, not a new
attribute/nominal-carrier bridge. No arithmetic information is lost in this
Int32 subset. Checked arithmetic, division, wider numeric types and general bodies
remain outside extended output; neoCLR owns writer/reader expansion, Raven owns
source emission and output selection. Unified main is used; no porting-branch
integration is implied. Use the matching arithmetic-capable neoCLR reader/writer.


### Extended CLI bitwise qualification (2026-11-14)

The matching neoCLR writer persists existing Int32 &, |, ^ and ~ plus Boolean
&, | and ^ as standard CIL and/or/xor/not. Raven syntax, binding, inference and
eager Boolean semantics are unchanged. Runtime Contract settings, ordinary .NET
behavior and native default output are unchanged. No compiler implementation
change or carrier encoding is needed; no information is lost for this subset.
neoCLR owns persistence and typed runtime admission, including a native verifier
fix for matching Boolean operands; Raven owns existing source emission and output
selection. Wider integers, enums, shifts and general bodies remain output gaps.

Compile neoCLR's samples/metadata-forwarding/Bitwise.rvn with direct rvnc neoclr
--extended-cli --library and --native-core-reference selecting System.Runtime.dll.
Int32 masks/complement/sign bits and Boolean truth tables run in both engines.
See neoCLR docs/validation/extended-cli-bitwise-20261114/ for commands, exact
revisions and hashes. Use matching bitwise-capable readers/writers. Work uses
unified main; no porting-branch integration is implied. Broader native CLI output
qualification remains separate from compiler integration status.


### Extended CLI signed-shift qualification (2026-11-14)

The matching neoCLR writer persists existing Int32 << and >> as ordinary CIL
shl/shr. Raven source binding, lowering, Runtime Contract settings, native default
output and .NET behavior remain unchanged. neoCLR owns persistence and runtime
admission; Raven already emits the corresponding operations. No carrier encoding
or information loss is introduced for this subset. Unsigned and wider shifts
remain outside this bounded output profile. Both engines retain five-bit Int32
count masking, including negative counts, and signed-right sign extension.

Compile neoCLR samples/metadata-forwarding/Shifts.rvn with direct rvnc neoclr
--extended-cli --library using the existing host-primitive bootstrap (no explicit
core reference) for this self-contained Int32-only fixture. The same source
compiled to default native output with the previous compiler is
compared against the extended image in both engines. Commands, revisions and
hashes are in neoCLR docs/validation/extended-cli-shifts-20261114/. Matching shift-
capable writer/readers are required. This qualification uses unified main, with
no porting-branch integration and no new compiler implementation change.


### Extended CLI signed-division qualification (2026-11-14)

The matching neoCLR writer persists existing Int32 / and % as standard div/rem.
Raven binding, lowering, Runtime Contract settings, native default output and .NET
behavior remain unchanged. neoCLR owns persistence and runtime admission; Raven
already emits these graph operations. No carrier or information loss is introduced
for this subset. Existing neoCLR terminal faults remain: DivideByZero for zero
divisors and ArithmeticOverflow for minimum/-1 division and remainder. The latter
remainder edge is not identical across all CLR implementations; this slice keeps
neoCLR behavior rather than changing source/runtime semantics.

Compile neoCLR samples/metadata-forwarding/Division.rvn using direct rvnc neoclr
--extended-cli --library and its existing host-primitive bootstrap. Compare with
previous-compiler native output of the same source in both engines. This fixture
is Int32-only; it does not qualify core-dependent consumers. Exact revisions,
hashes and commands are in neoCLR docs/validation/extended-cli-division-20261114/.
Unsigned/wider division and comparisons remain output gaps. ExtendedCliChecks now
uses comparison to check atomic NEOMETA003 rejection and successful native output.
Work uses unified main, with matching writer/readers and no porting-branch merge.


### Extended CLI comparison qualification (2026-11-14)

The matching neoCLR writer persists existing Int32 ==, < and > and Boolean ==
as standard ceq/clt/cgt. Boolean results and locals remain Boolean in neoCLR.
Raven binding, lowering, Runtime Contract configuration, native defaults and the
.NET backend are unchanged. No carrier or signature extension is introduced.
neoCLR owns persistence and typed admission; Raven already emits the operations.
The ordinary scalar CLI loader separately adapts results to Int32 0/1; general
CLI evaluation-stack normalization remains a gap in typed loading.

Compile neoCLR samples/metadata-forwarding/Comparisons.rvn with direct rvnc neoclr
--extended-cli --library, using the host-primitive bootstrap for this self-contained
fixture. Compare with previous-compiler native output in both engines. Commands,
exact revisions and hashes are in neoCLR docs/validation/extended-cli-comparisons-20261114/.
Boolean literals/negation, <=/>=/!= lowering, wider/unordered/reference comparisons
and branches remain output gaps. ExtendedCliChecks now checks atomic rejection of
Boolean negation and successful native emission. Unified main is used with matching
writer/readers; no porting-branch integration or core-dependent qualification.


### CLI Boolean stack alignment (2026-11-14)

The matching neoCLR metadata writer and runtime now use CLI Int32 evaluation
values for Boolean loads, literals and comparison results. Declared Boolean
signatures, boxing and storage retain their identity; stores truncate to a byte,
loads zero-extend. This supersedes the previous typed-Boolean workaround and
comparison adapter. Existing Raven !, <=, >= and != lowering requires no compiler
change; ordinary .NET semantics and Runtime Contract configuration remain unchanged.

neoCLR owns the C# graph verifier, persistence, runtime verifier and both engines.
Native output remains the default. Extended output is opt-in; no new temporary
carrier or #Nex flag is introduced. ExtendedCliChecks uses a conditional branch
for atomic NEOMETA003 rejection, also checking that native output still emits it.
Broader extended branch ingestion and wider/unordered/reference comparisons are
still gaps, not permanent source restrictions.

Qualification uses unified main and neoCLR samples/metadata-forwarding/Booleans.rvn,
compiled through the self-contained host-primitive bootstrap into both native and
extended output. Both run in interpreter and native arm64. Reproduction, revisions
and hashes: neoCLR docs/validation/cli-boolean-stack-20261114/. No porting-branch
merge or broader source-core/experimental capability qualification is claimed.

### Extended CLI branch qualification (2026-11-14)

The matching neoCLR metadata writer now persists existing Raven labels and
br/brtrue/brfalse lowering with standard CLI relative offsets. The nominal/extended
reader validates typed CFG joins, assignment and backedges; both engines use their
existing branch instructions. Raven binding, short-circuit behavior, loop control,
Runtime Contract settings, native defaults and the .NET target are unchanged.
No carrier, signature extension or #Nex feature is introduced. neoCLR owns encoding,
reader verification and runtime lowering; no compiler semantic change is needed.

The sample neoCLR/samples/metadata-forwarding/Branches.rvn covers conditionals,
early returns, loops, break/continue and short-circuit expressions that skip or
execute division by zero. Native output from the previous compiler and extended
output from the matching writer run in both engines. Reproduction and revisions
are in neoCLR/docs/validation/cli-branches-20261114/. The self-contained primitive
fixture uses host-primitive bootstrap on unified main, without a porting-branch merge.

ExtendedCliChecks now accepts branches and checks atomic NEOMETA003 rejection of
Int64 signatures while native emission still succeeds. Direct comparison branches,
switch, exception flow and broader body/type admission remain separate gaps,
not source-language restrictions or broader capability qualification.

### Comparison branch qualification (2026-11-14)

The matching neoCLR metadata writer now fuses adjacent Ceq/Clt/Cgt and
Brtrue/Brfalse into signed I4 comparison branches. It never fuses across labels.
Raven binding, semantic lowering, Runtime Contract settings, native defaults and
ordinary .NET emission are unchanged; no compiler source change is needed.
neoCLR owns persistence, verification and expansion into existing runtime branches.
No new bridge carrier, signature or #Nex extension is introduced.

The neoCLR sample `samples/metadata-forwarding/ComparisonBranches.rvn` checks all
six relations with equality, negative values and Int32 extremes: 36 inputs in each
of native and extended formats in interpreter and native arm64. The primitive
bootstrap uses unified main, not a porting branch or broader source-core support.
See neoCLR `docs/validation/cli-comparison-branches-20261114/` for reproduction,
matching revisions and hashes. Native baseline uses the preceding compiler.
Unsigned ordering, floating/unordered and reference comparisons, switch and EH
remain metadata-profile gaps, not new Raven language restrictions.

### Unsigned branch qualification (2026-11-14)

The matching neoCLR metadata reader/writer supports standard UInt32 signatures
and unsigned I4 comparison branches, including standalone clt.un/cgt.un. Adjacent
comparison/branch pairs fuse without crossing labels. UInt32 signature/storage
identity remains distinct from the I4 evaluation category. No new #Nex extension,
bridge carrier, runtime opcode or Raven semantic lowering is introduced.

`neoCLR/samples/metadata-forwarding/UnsignedBranches.rvn` checks all six relations
with zero, equality, UInt32.MaxValue and both sides of the sign bit, plus UInt32
locals, calls and returns. Native baseline and extended images run unchanged in
both engines: 36 inputs per format (72 per engine). Evidence and hashes are in
neoCLR `docs/validation/cli-unsigned-branches-20261114/`. This uses unified main's
host-primitive bootstrap, without a porting-branch merge or source-core qualification.
Runtime Contract settings, native defaults and ordinary .NET behavior are unchanged.
Floating/unordered and reference comparisons, switch and EH remain profile gaps;
UInt32 union alternatives remain outside the existing scalar-union profile.

### Floating branch qualification (2026-11-14)

The matching neoCLR metadata library admits Single/Double signatures and ldc.r4/
ldc.r8 with ordered/unordered CLI comparison branches. Typed fusion accounts for
NaN: floating clt + brfalse maps to bge.un, clt.un + brfalse maps to bge, and cgt
has corresponding ble mappings. Integer mappings stay unchanged. Graph validation
normalizes Single/Double to F while retaining declaration/storage types. neoCLR
owns this persistence and verification work; no Raven source lowering changes.

The author reaffirmed that Raven should use the same IL instructions and .NET
semantics unless a justified intentional divergence exists. This closes admission
gaps without a new runtime instruction, #Nex extension or bridge carrier. Runtime
Contract settings, native defaults and ordinary .NET emission remain unchanged.

neoCLR `samples/metadata-forwarding/FloatingBranches.rvn` exercises six relations
for Single/Double with NaN in either/both operands, signed zero, infinities and
finite values, plus local/call/return boundaries. There are 32 invocations per
engine across native/extended images, exercising 192 comparisons. C# authoring
also runs against .NET. Revisions, hashes and results are in neoCLR
`docs/validation/cli-floating-branches-20261114/`. The primitive bootstrap uses
unified main, without a porting-branch merge or broader source-core qualification.
Reference comparisons, switch, EH and broader floating bodies remain gaps.

### Reference comparison branches (2026-11-14)

The extended/nominal body reader accepts beq/bne.un (short/long), ceq and conditional
brtrue/brfalse on admitted object references. Equality compares identity, not contents.
The existing runtime ReferenceEqual/ReferenceIsNull operations provide execution;
no new execution opcode, metadata signature or #Nex flag is needed. Object ordering
branches and mixed integer/reference operands reject. Managed pointers and structural
value unions are not treated as object references by this admission.

Standard ldnull is admitted for comparisons, null tests and dup/pop. Its untyped
null marker is consumed during lowering without allocating a runtime object.
General null storage, call/return and null/reference control-flow joins remain gaps.
The bounded cgt.un reference case requires a literal null right operand and means
non-null; other reference cgt.un pairs remain unqualified. These are admission limits,
not alternative .NET semantics. Existing typed null references can flow through
already supported signatures/locals/calls independently of literal-null admission.

C# Ceq authoring verifies object operands and writes ceq/beq/bne.un for CLI or the
existing ref.eq for native images. Raven's portable backend gains an explicit,
default-off AllowsArrayReferenceEquality capability enabled by neoCLR. It lowers
built-in array ==/!= only when no operator method is bound and both operand types
are admitted. Overloaded equality, string equality and ordinary .NET emission are
unchanged. Prior neoCLR rejection was a bridge gap, not Raven language semantics.

`ReferenceBranches.rvn` tests same allocation, distinct equal-content arrays, both
null and either-null pairs: 15 cases per format, 30 per neoCLR engine, plus 15 cases
compiled/executed with Raven's .NET target. Native uses the existing reference-arena
profile with the explicit test foundation. Runtime Contract settings are unchanged.
See neoCLR `docs/validation/cli-reference-branches-20261114/`.

Work is on unified main with explicit neoCLR capability ownership. The sample uses
host-primitive bootstrap; no porting-branch merge or broad source-core qualification
is claimed. Class/interface built-in equality is a deferred general candidate;
array support does not authorize unrelated structural or overloaded-equality work.
