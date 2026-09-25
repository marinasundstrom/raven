# General compiler fixes extracted from the neoCLR experiment

2026-09-14. Branch `codex/compiler-fixes-integration`, based on upstream main
`d92b02812740ae052f277c23151e9cc208f7672d`. The experimental target branch remains
separate. This branch does not add neoCLR target selection, array invariance,
Void-as-a-generic-argument policy or runtime protocol renaming.

## Reviewed batches

| Batch | Original experiment commits | Ordinary .NET evidence |
| --- | --- | --- |
| Numeric operations, operator applicability and pointer substitution | `26907410f`, `c1431bea1`, `406962312`, `09cf60417`, `bc0ec8046`, `809aef0fe`, `e51da7a48` | 122 selected tests: 12 failures before applying compiler fixes; all pass afterwards. |
| Binding, dispatch, receivers, enum context and constructor completion | `a843844e4`, `6ab473fbc`, `9b269f9d0`, `62105de24`, `3142f2f13`, `55c0f7ef5`, `854cd4d3d` | 32 selected tests: nine failures before this batch; all pass afterwards. |

Changes and relevant specs were applied individually; this is not a merge of the
experimental branch. Tests for interface flags, imported indexers and expression
receivers were separated from target-specific fixtures. The method-group regression
was extracted without bringing in an unrelated explicit-Void delegate test.

The checks cover observable CLR execution, metadata flags, conversion classification,
invalid operator operands, inherited-interface ambiguity and ordinary semantic-model
completion. They do not establish that every changed path independently failed before
its fix. No emitted-opcode sequence assertions were introduced.

The fresh worktree ran `scripts/codex-build.sh`. Initial focused tests built their
project references; subsequent focused tests reused those foundational library builds
and rebuilt the compiler/test assemblies as needed. Host SDK:
`11.0.100-rc.1.26425.128`; focused execution target: `net11.0`.
After both batches, `scripts/test-baseline.sh` completed successfully: 5,489
passed, no failures or skips (including its compiler, editor and supporting test
projects). The focused execution checks above cover the changed runtime paths.
This is not a full Raven release gate or validation of NanoFramework. The branch
is prepared for review; this record does not claim it has been merged into main.

The language specifications updated with the fixes cite the C# numeric-conversion
and operator-applicability baselines. These corrections restore ordinary .NET
behavior; the experiment discovered them but does not own their semantics.

## Main integration directive — 2026-09-14

The author directed that fixes benefiting Raven generally belong on Raven main,
while experimental neoCLR support stays on a separate feature branch. This is the
workflow for future fixes too: extract and validate general behavior independently;
do not merge the experimental branch wholesale.

The namespace fixes from `f80902d70` were extracted onto this main-based integration
branch. The regression was rewritten with ordinary .NET framework references and
default compilation options, removing its dependency on experimental MetadataImportOptions.
It failed on imported-member completion before the fix. Metadata-first marker lookup
also uses the existing general reference-type resolver; no new target option is added.
The original numeric/binding batches remain unchanged.

General cross-target emission candidates listed in neoCLR's assessment still require
individual dependency review and ordinary-target evidence. They are not classified as
permanently experimental merely because neoCLR discovered them. Iteration/propagation
renaming, alternative array semantics and inhabited Void remain on the experiment
branch until separately designed and approved for Raven's general target model.

Main-integration validation: 47 focused namespace tests passed, followed by
`scripts/test-baseline.sh`: 5,490 passed, zero failures/skips. The latter includes the
new default-options namespace regression. The prior numeric and binding runtime
checks remain the evidence for those unchanged batches. This is source integration,
not a Raven release or NanoFramework certification.

## Pointer metadata emission — 2026-09-14

Extracted the pointer reconstruction fix from `3df1b54b0` on the main-based
`codex/general-pointer-emission` branch. The original default-options test already
passed on main, so it was extended to exercise the existing EmitOptions target-core
contract as well. That case failed with an unsupported `Unit*` metadata type before
the fix (six other normalizer checks passed). The change recursively preserves
pointer element types during method-reference reconstruction. It introduces no
experimental metadata import options or neoCLR-specific type semantics.

Validation: all 53 focused normalizer, pointer code-generation, pointer semantic
and pointer syntax checks passed. The compiler build and whitespace formatting
completed. This scoped check is not a new full release gate.

## Closed-generic reference metadata — 2026-09-14

Extracted `17c9f8b82` on `codex/general-generic-metadata`, based on main after the
pointer fix. The regression uses normal .NET references and default CompilationOptions;
it does not import the experimental MetadataImportOptions contract. Before the fix,
emission failed while resolving the reference-only `Contracts.Container<T>` type.
Afterwards, all 18 focused metadata and generic-invocation tests passed.

When EmitOptions selects a target core library, named metadata types and their
closed constructions remain in the metadata context. Generic member references
preserve definition parameters on a constructed owner, nested value types retain
their value-type flag, and method proxies preserve by-reference parameter shapes.
Default emission policy is unchanged. The metadata test checks signatures, locals,
nested generic getters and an out-parameter union extractor; it does not execute a
reference assembly. Mixed source/metadata constructions, constructors and generic
methods still require the subsequent independent reviews.

The repository target-framework matrix passed with SDK `11.0.100-rc.1.26425.128`,
building the libraries and building/running representative .NET 10 and .NET 11
projects with the repository toolchain. This is not the full Raven release gate.

## Reference-only constructors — 2026-09-14

Extracted `995a4c982` on the main-based `codex/general-constructor-metadata`
branch. Extending the normal-reference fixture to construct nested generic cases
and their carriers reproduced a MetadataLoadContext mismatch before the fix.
Target-core emission now allocates temporary constructor tokens and rewrites them
to the original metadata signatures in the final PE. Temporary proxy types are
removed; default constructor resolution remains unchanged. Tests assert the final
constructor signatures and absence of the temporary types, not opcode sequences.

Completed integration branches `codex/compiler-fixes-integration`,
`codex/general-pointer-emission` and `codex/general-generic-metadata` were deleted
locally and remotely after confirming they were ancestors of main. Superseded
`codex/neoclr-target-contracts` and `codex/neoclr-target-resolution` were likewise
removed after confirming their history is contained in the active
`codex/neoclr-namespace-metadata` experiment. Unrelated branches were retained.

Validation: 26 focused metadata, generic-invocation and constructor checks passed,
as did the repository .NET 10/.NET 11 build/run matrix with SDK
`11.0.100-rc.1.26425.128`. This is a scoped integration, not a full release gate.

## Closed generic method calls — 2026-09-14

Independently extracted `11e9964f2` on `codex/general-method-metadata` from main.
The regression uses ordinary .NET references and default CompilationOptions with
the existing EmitOptions target-core setting. Before the fix, calling `Echo<int>`
from a metadata-only assembly failed with a MetadataLoadContext mismatch; the
existing generic-type/constructor test still passed.

Closed generic metadata calls now use method proxies whose final MethodSpec retains
concrete type arguments and the generic definition's parameter/return signature.
Assembly scope normalization includes those method type arguments. Open generic
arguments continue through the existing resolver. Tests inspect metadata identity
and generic parameter kind/position, rather than requiring a particular opcode
sequence. This adds no neoCLR target configuration or language syntax.

Validation: all 27 focused metadata, generic-invocation and constructor checks
passed. The repository .NET 10/.NET 11 build/run matrix passed with SDK
`11.0.100-rc.1.26425.128`. This is not the full release gate.

## Closed generic fields — 2026-09-14

Independently extracted `4af98e7c1` on `codex/general-field-metadata` from main.
The default-options, ordinary-reference fixture reproduced a MetadataLoadContext
mismatch when reading `Box<int>.Value` before the fix. Field tokens now preserve
the closed declaring type and the generic definition's field signature. The
regression also covers static reads/writes and checks that temporary proxy types
are removed from the final assembly. Default emission uses the existing resolver.

The author explicitly clarified that fixes benefiting Raven on .NET Framework or
NanoFramework also belong on main. Repository instructions now preserve this
boundary: general compiler fixes on main, neoCLR-specific integration on its
experimental branch. Test results must identify the actual target; modern .NET
matrix success does not claim .NET Framework or NanoFramework execution.

Validation: all 22 focused metadata, field and generic-invocation checks passed.
The repository .NET 10/.NET 11 build/run matrix passed with SDK
`11.0.100-rc.1.26425.128`. No .NET Framework or NanoFramework runtime test was run.

## Generic delegate construction — 2026-09-14

Extracted `5f274c063` independently on `codex/general-delegate-metadata` from main.
The normal-reference, default-CompilationOptions regression failed before the fix
while constructing a metadata-only `Callback<int>` delegate. Delegate normalization
now preserves an existing metadata constructor proxy instead of re-resolving it
through reflection and mixing compiler-host and metadata-context types. The final
constructor retains its target assembly, closed owner and Object/IntPtr signature.
This is a general emitter correction and introduces no neoCLR-specific mapping.

The intended target architecture is reusable compiler mechanisms with explicit
framework-contract mappings; neoCLR's experimental configuration and unresolved
semantics stay separate. A compiler defect discovered through neoCLR remains a
general fix when the same metadata contract applies to other frameworks.

The author further clarified that neoCLR-specific tests and mappings must not enter
main yet. These fixtures use ordinary CLI metadata contracts and no neoCLR options.
A possible future emission backend is an open evaluation, outside this stabilization.

The delegate candidate passed 26 focused tests and the modern .NET target matrix
before it was set aside for the [main stability audit](main-stability-audit.md).
That audit found and corrected an attribute serialization regression at `5a67d5d4c`;
the delegate fix was then restored on top for combined focused validation.

Combined validation after restoring the delegate fix: 55 focused metadata, attribute,
delegate and generic-call tests passed. The broader audit remains tied to its stated
commits; it is not reported as a rerun on this delegate commit.

## Direct out-parameter forwarding (2026-09-19)

A direct invocation that passes an enclosing out parameter to a callee out parameter
establishes assignment on normal return. Previously the method-body checker tracked
explicit assignments but missed this call guarantee, producing RAV0269. The fix uses
CLI parameter ref kinds; ref/in arguments do not provide the same guarantee.

The independent regression uses a source generic setter and .NET Math.DivRem,
then executes the emitted code and observes 42. It failed with RAV0269 before the
fix. All 41 focused parameter semantic/runtime checks pass, including deferred and
conditional forwarding that must still report missing assignment. This is limited
to direct invocation expressions; it does not redesign flow analysis for calls
nested in conditional expressions. No Runtime Contract option or neoCLR policy is
introduced. .NET Framework and NanoFramework were not executed.

## Same-name constructor arities (2026-09-19)

Unqualified constructor lookup must retain all accessible named types in the current
namespace before choosing a generic arity. A namespace containing both Box and
Box<T> previously selected Box alone: Box<T>.Create calling Box<T>(value) silently
bound an error expression and returned a default value. The independent ordinary
.NET execution regression returned 0 before the correction and 42 afterward.
Invalid constructor arities and extra type arguments on a constructed alias now
produce RAV0305, preventing emission of these unresolved expressions.

The fix changes candidate discovery and diagnostics, not emitted metadata contracts.
It introduces no Runtime Contract setting or neoCLR policy. All 78 focused generic,
alias, namespace and accessibility checks pass on .NET 11. .NET Framework and
NanoFramework were not executed. Namespace access checks and candidate deduplication
remain in force; this does not claim to resolve every nested-case lookup issue.

## Enum backing-field metadata (2026-09-19)

The final PE metadata pass preserves `SpecialName | RTSpecialName` on the instance
`value__` field of CLI enums, as required by ECMA-335 II.14.3. Raven already requests
both bits when defining the field, but the host PersistedAssemblyBuilder masks
reserved field attributes. The correction applies to normal and explicitly retargeted
emission, including nested enum definitions, without changing source semantics,
Runtime Contract configuration or underlying integral storage.

An independent ordinary .NET regression inspects the emitted PE field attributes,
then loads the enum and checks its underlying type and literal value. It failed
before the correction; all 13 focused enum/target-core checks pass on .NET 11.
.NET Framework and NanoFramework execution are not established by these checks.
The final metadata normalization is necessary while the host emission layer drops
the bit; it does not admit malformed enums or add target-specific enum policies.

Sources: [CLI standard](https://ecma-international.org/publications-and-standards/standards/ecma-335/),
[Persisted field builder](https://github.com/dotnet/runtime/blob/main/src/libraries/System.Reflection.Emit/src/System/Reflection/Emit/FieldBuilderImpl.cs).

## Imported library nested case ownership (2026-09-19)

The neoCLR library importer now retains lexical nesting for the admitted
Option/Result companion cases. Raven's emitted CLI nested-type relationship was
previously flattened into a dotted neoIL name, losing declaring-type identity.
The correction is entirely in neoCLR's target importer; no Raven semantic or
emission change, Runtime Contract option, or general nested-type admission is added.
All 73 source slices regenerate and all 12 union admission cases pass. The runtime
regression compares each of the four cases with its resolved owner definition.

## Sealed introspection consumers (2026-09-19)

neoCLR's six Info contracts are now Raven sealed interfaces with internal providers.
Its generated reference preserves ClosedHierarchyAttribute and hidden permitted
provider definitions, allowing Raven to resolve the closed family. MemberInfo's
three direct cases are FieldInfo, MethodInfo and PropertyInfo; the shared runtime
storage base is not an additional public case. The target importer validates this
family and rejects foreign implementations independently of source diagnostics.

An external consumer's three interface arms are exhaustive and run against real
snapshots. Removing each arm yields RAV2100. neoCLR now imports the emitted isinst,
reference branches and non-null tests; callers rebuild for interface dispatch.
There is no new Runtime Contract option or Raven semantic/emission change.
Reference projection normalizes C#'s inherited accessor virtual/final flags to the
ordinary Raven storage-base methods, with exact admission checks retained.
All 73 source slices regenerate, 12 descriptor admission cases pass, and language
server completion identifies all six contracts as interfaces. Raw neoIL does not
acquire general sealed-hierarchy enforcement from these target checks.

Deferred general candidate: RAV2100 currently names inaccessible concrete leaves
rather than accessible covering interface cases. Improve diagnostic/code-fix case
selection independently on ordinary CLI metadata before integration on main; do
not add hard-coded neoCLR names to Raven. TypeInfo acquisition, RuntimeContext and
collection-return alignment remain separate neoCLR work.

## Unified neoCLR TypeInfo acquisition (2026-09-19)

The neoCLR consumer profile now selects the existing Runtime Contract resolver:
`RavenTypeOfAssemblyName=NeoCLR.CoreProbe`,
`RavenTypeOfInfoType=System.Introspection.TypeInfo`, and
`RavenTypeOfContextType=System.Runtime.RuntimeContext`. Raven binds typeof to TypeInfo
and emits Current followed by GetTypeInfoFromHandle. Library declaration slices
clear this configuration because they shadow those types and use no typeof.

neoCLR also implements Object.GetType returning TypeInfo and removes its public
Type/Info hop. The reference retains an internal empty System.Type shell solely
for CLI custom-attribute type tokens; it is not an executable/public runtime type.
No Raven compiler change or main-branch fix accompanies this migration. Importer
admission still validates sealed interfaces and hidden providers. Target consumers
must rebuild against the changed reference and executable library together.
RuntimeContext.ExecutingAssembly and assembly/module discovery remain subsequent
neoCLR work; this change does not implement invocation or Emit.

Validation: TypeInfo and Object admission checks pass; a saved Raven program
executes instance and declared-type acquisition. Language-server checks identify
all six Info contracts as interfaces, exclude System.Type and private providers,
and expose RuntimeContext.Current.GetTypeInfoFromHandle without Type.Info.

## Source-origin metadata for neoCLR discovery (2026-09-19)

neoCLR's target importer now preserves descriptive assembly/module identities and
CLI definition tokens for admitted application types/methods, including field and
parameter rows. Bootstrap/primitive reference scopes map to the logical System.Runtime
assembly. Compiler-generated target adapters retain no source origin; separately
compiled library slices do not copy colliding source token rows into the merged
runtime module. Execution still binds through checked signatures and its existing
runtime definition IDs. No Raven compiler or Runtime Contract change is required.

Eighteen neoCLR metadata/attribute/scoping checks and a saved acquisition sample
pass. Public assembly discovery/token interfaces remain subsequent target work.
The descriptor properties must use module-scoped identity, not DefinitionIndex.


## neoCLR minimal discovery and token contracts (2026-09-19)

The target's RuntimeContext now exposes ExecutingAssembly. Sealed AssemblyInfo and
ModuleInfo interfaces expose loaded metadata through Sequence<T> collections;
ReferencedAssemblies reports direct dependencies, including the mapped System.Runtime
foundation. Info interfaces expose MetadataToken, with module ownership on type,
member and parameter contracts. Native snapshots materialize private Raven providers.

Runtime Contract configuration is unchanged: typeof still uses the configured
TypeInfo/RuntimeContext handle resolver. No Raven binding or emission change is
needed; the target reference/importer/provider contracts change together and callers
must rebuild. CLI source tokens survive target import; merged runtime definitions
receive scoped tokens separately. Discovery only covers retained loaded definitions;
unavailable references and open-generic member queries fail explicitly. Future dynamic
loading belongs to RuntimeContext, as the author reaffirmed; none is added now.

Validation: the saved Raven sample traverses Demo → System.Runtime and its module's
Widget type. Language-server checks identify all eight Info contracts as interfaces
and expose ExecutingAssembly, token/module properties and Sequence capabilities.
Target runtime checks cover dependency callers, token preservation, generic definition
discovery and resource limits; implementation admission rejects altered layouts.

Deferred general candidate observed while writing the sample: reusing a for-loop
variable name for a later local produced RAV0174 inside the earlier loop. The sample
uses distinct descriptive names. This has not been isolated on ordinary CLI metadata
or diagnosed as a compiler defect, so no fix or main integration is claimed.


### Complete neoCLR Introspection Sequence results

The author subsequently requested migrating every remaining Introspection collection
result from arrays to Sequence<T>. TypeInfo generic arguments, interfaces, enum names,
fields, methods and properties now use Sequence, as do MethodInfo parameters and
PropertyInfo index parameters. The target core reference and provider implementations
move together. Runtime Contract settings, Raven semantics and emission are unchanged;
existing array-to-interface conversion supplies the public collection capability.

Consumers rebuild, annotate Sequence<Element> and use Count instead of Length.
Indexing, iteration and Iterable query extensions remain supported; mutation and
implicit array assignment are rejected. Native metadata services keep private array
storage, with independent snapshots. Editor checks cover all result families and
saved programs exercise filtering, indexing, enumeration and query extensions.
No target policies or compiler changes are integrated into main for this slice.

### TypeInfo joins MemberInfo (2026-09-19)

The neoCLR reference/importer/provider contracts now make TypeInfo the fourth sealed
MemberInfo case. Name, Module and MetadataToken are inherited; DeclaringType returns
Option<TypeInfo>. Consumers rebuild and update exhaustive matches and owner access.
All member cases are authored together to satisfy Raven's existing same-source-file
rule. Runtime Contract configuration, binding and emission rules are unchanged;
there is no compiler fix or main integration in this slice.

The target importer preserves nested CLI declaring-type tokens and retains owners.
neoCLR validates scoped ownership, missing parents and cycles. Saved Raven programs
exercise nested/top-level ownership and four-case matching; source admission and
runtime metadata tests cover the changed contracts. Nested enumeration and dynamic
assembly loading remain deferred.

Target importer follow-up: source-origin parameter tokens now include a zero slot
for an instance receiver when its method is emitted as a free function, including
value-type constructors. The CLI receiver has no Param row; declared parameter
tokens keep their order. This fixes neoCLR metadata admission for ordinary Raven
value-copy programs. Runtime Contract settings and Raven compiler emission are
unchanged; the target importer owns the adaptation.

### 2026-09-19 — Minimal UTF-8 boundary API

The neoCLR target reference catalog now exposes System.Text.Utf8.Encode(String)
as Sequence<Byte>, Decode(Sequence<Byte>) as Result<String,InvalidUtf8Error>, and
String.IsEmpty as a property. Rebuild target metadata and runtime together; change
IsEmpty() callers to IsEmpty. Target adapters snapshot managed bytes; strict UTF-8
validation stays in neoCLR native services. No Raven semantic/emission changes,
new keyword, or Runtime Contract configuration changes: unit/() maps to System.Void.
The target remains isolated on neoclr; no compiler fix needs extraction to main.
The signature probe and editor completion cover the new declarations; UTF-8 sample
checks include malformed input, BOM/NUL preservation and snapshot independence.
Encoding hierarchies, Utf8String, streaming and scalar Char remain deferred.


### 2026-09-24 — neoCLR String sequence construction

neoCLR target metadata now exposes String(Sequence<Char>), a read-only indexer and
String as Sequence with explicitly implemented Collection.Count. Length remains
public. The neoCLR importer validates the constructor and lowers it to a managed
snapshot factory; private explicit Count is matched by MethodImpl identity. String
conversions to Collection/Sequence emit the required target interface cast.

No Raven semantic/emission implementation or Runtime Contract configuration changes
are made. Rebuild matching target metadata, bridge and runtime. The neoCLR
string-sequence sample validates construction, copying, indexing and interface-only
Count; direct Count and index mutation are compiler errors. Keep this target work
on neoclr; there is no general compiler fix to extract here. The installed compiler
cannot emit String([]) directly (empty Sequence-target collection expression);
a typed empty char array works. Investigate that general candidate independently
before proposing a Raven main fix. Iterable construction remains exploratory.


### 2026-09-24 — Explicit String interning

neoCLR target metadata adds String.Intern(String) -> String. Its importer validates
the exact static signature; the Raven-authored body calls a trusted runtime service.
The runtime pool belongs to one execution, not immutable loaded metadata or a shared
process/session. Independent invocations and isolated workers have separate pools;
entry/payload quotas raise InternPoolLimitExceeded when adding a new value over budget.
Existing references are unchanged, and explicit host-shared inputs retain normal aliases.

No Raven syntax, literal emission, automatic interning or Runtime Contract configuration
changes are made. Rebuild the matching target reference, bridge and runtime. The neoCLR
String interning sample checks canonical returns, original references, exact text and GC;
bridge checks reject wrong signatures/static callvirt. This is target integration on
neoclr; no general compiler fix is being merged into main.


The same target update aligns String parameter names across reference and implementation:
constructor characters; Intern text; Concat/CompareOrdinal left/right; Equals other;
ContainsOrdinal substring; StartsWithOrdinal prefix; EndsWithOrdinal suffix;
SliceUtf8 byteStart/byteLength. Indexers keep index. Positional signatures are unchanged,
but named callers using value0/value1 must migrate. The String sequence sample checks
reordered named arguments and rejects the old generic names. This is target metadata
maintenance, not a Raven language change.


### 2026-09-24 — Provisional neoCLR TCP client

The target reference now contains System.Networking.Sockets.Socket.Connect(string,
int) -> Task<Result<Socket, SocketError>>, Receive(byte[], int, int) ->
Task<Result<int, SocketError>>, and Close(). SocketError uses the existing imported
union-carrier shape. The handle constructor and completion classes are internal.
The neoCLR importer validates these selected signatures and emits calls to the
Raven-authored library; private RuntimeServices bridge to nonblocking TCP completion.

No Raven language, semantic model, state-machine emission or Runtime Contract
configuration changes are made. Await still uses the target Task/Promise builder
protocol and generated state machines. Rebuild matching core metadata, importer,
bootstrap library and runtime. Do not merge this target metadata work into main.

Validation in neoCLR's docs/experiments/socket-client runs a real loopback greeting,
short reads/EOF, close and allocation churn: 914 objects reclaimed, zero live and
22 collections. Compiler checks reject the handle constructor and completion class.
The local target test is macOS evidence, not other-platform or .NET evidence.

Two open integration limits were encountered: direct `error is SocketError.Closed`
value-type testing is not admitted by the target importer; the sample uses IsClosed.
A Result local hoisted across an additional await has non-defaultable carrier storage
in a generated heap state-machine constructor, which the runtime rejects. The demo
checks the already-completed Closed result without another await; the general
hoisted-union initialization contract remains open. Do not weaken runtime constructor
checks or claim a compiler fix. The author prioritizes provisional interfaces for
an HTTP web-app demo; full networking and TcpClient/UdpClient layers are not gates.


### 2026-09-24 — neoCLR client send and echo

The target adds Socket.Send(byte[], int, int) -> Task<Result<int, SocketError>>.
The provisional library snapshots the selected bytes, reports short sends and
permits one pending transfer per direction. Private SocketReceiveCompletion and
SocketReceiveResult become SocketTransferCompletion and SocketTransferResult;
rebuild matching target reference, importer, library and runtime. There is no
compiler implementation, state-machine emission or Runtime Contract change.

The socket sample uses the existing precedence fix: `await Foo()?` propagates
the awaited result, just as `try Foo()?` propagates the complete try expression.
Refresh older local compiler bundles before checking this syntax. The real TCP
echo run reclaims all 1,380 allocations in 30 collections, with zero live objects;
both private-constructor/completion visibility checks pass. Existing direct case
test and hoisted-Result limitations above remain open. Target evidence is local
macOS, not a release or general compiler compatibility claim.


### 2026-09-24 — neoCLR hostname lookup

The neoCLR target adds System.Networking.Dns.GetHostAddresses(string) returning
Task<Result<Sequence<string>, DnsError>>. Address strings are numeric IPv4; connection
remains a separate Socket operation. The private DnsCompletion and runtime helpers
convert owned host results into a managed array view on the VM owner. Rebuild matching
reference metadata, importer, library and runtime. There is no Runtime Contract,
compiler semantic policy or state-machine emission change in this slice.

The compiled localhost echo sample passes with 1,428 allocations reclaimed across
31 collections, zero live objects and three rejected private-type/constructor uses.
This is local macOS target integration evidence, not a Raven release or .NET check.

Deferred general-fix candidates: using Task<Result<string[], DnsError>> in the
library triggered System.ArgumentException: System.String[] was not loaded by the
MetadataLoadContext that loaded the generic type or method, at
TypeSymbolExtensionsForCodeGen.GetClrTypeInternal / RoDefinitionType.MakeGenericType.
Sequence avoids that code path and is the selected read-only consumer contract;
it does not fix generic array emission. Separately, nesting an OnCompleted callback
capturing its newly created exchange task inside another callback produced a null
capture at Task.GetResult. A named StartExchange function avoids the nested shape.
Independently reproduce both with ordinary .NET metadata before locating/fixing the
compiler or importer issue; do not merge target-specific experiments into main or
relax neoCLR null checks. Earlier direct-case and hoisted-Result limits remain open.


### 2026-09-24 — neoCLR listener and accepted sockets

The target adds Socket.Listen(string,int,int) -> Result<Socket,SocketError>,
Accept() -> Task<Result<Socket,SocketError>> and GetLocalPort() -> Result<int,SocketError>.
The importer matches these signatures; private SocketConnectCompletion gains
StartAccept and reuses the result bridge. SocketError adds AddressInUse and
InvalidOperation. Refresh reference/importer/bootstrap/runtime artifacts together.
Compiler semantics, state-machine emission and Runtime Contract settings are unchanged.

A compiled server and client run as distinct neoCLR processes, using an OS-selected
loopback port and a localhost lookup. Both exchange Hi and release all managed
objects: server 104 allocations/three collections, client 1,428/31. Separate host
callbacks let the sample avoid the already-recorded nested capture failure. This
is local macOS integration evidence, not a compiler fix or a runtime release.


### neoCLR address-sequence connection target — 2026-09-24

neoCLR adds `Socket.Connect(Sequence<string>, int)` alongside the numeric-address
factory. Its library snapshots indexed values synchronously into managed storage,
then the private SocketConnectAddresses service retains parsed IPv4 endpoints.
The importer/reference admit the exact overload and keep completion helpers and
runtime operation IDs internal. All attempts share five seconds, with one second
per pending address while alternatives remain. DNS has a separate bound.

Runtime Contract configuration, compiler semantics and async state-machine emission
are unchanged. Match the neoCLR compiler reference, bridge, generated library and
runtime when testing; this is target integration, not a general Raven compiler fix.
The echo POC prepends an unavailable loopback address, mutates the source collection
after submission and exercises collection while pending. Existing nested-capture
and hoisted non-defaultable Result limitations remain open; this slice does not
change their workaround or establish a fix. Validation results are recorded in
neoCLR's socket API design and experiment documentation.

Local macOS integration passes: separate server/client processes exchange Hi, the
client records 43 collections and both finish with zero live managed objects. The
library regenerates and its snapshot validates; no compiler code changes were needed.


### neoCLR HTTP handler experiment — 2026-09-24

The neoCLR importer now permits a nested generated async state to access its
containing handler's private fields. Cecil-level positive/negative checks preserve
unrelated private access and readonly-write rejection. This changes the target
importer's admission behavior, not Raven emission or Runtime Contract configuration.
The nested-access correction itself changes no state ABI. The HTTP integration below
adds public core reference contracts and requires matching runtime-library artifacts.

The initial application-local HTTP experiment required a separate referenced contract
library: async Task results containing a source-defined HttpResponse were rejected
with RAV2704 while the referenced type succeeds. The integrated implementation now
provides HttpClient, HttpHandler, HttpSocketHandler, HttpRequest, HttpResponse,
HttpContent and HttpHeader through System.Web.Http in the neoCLR core reference.
A private callback-based exchange avoids that source-defined async result shape.
Match reference, bridge, generated library and runtime artifacts. Two other emitted
shapes remain unresolved: propagation directly into a field assignment leaves a
receiver on the error-return stack, and a hoisted non-null Sequence local is cleared
with null. Locals-before-assignment and a Sequence parameter in an async send helper
avoid these shapes without relaxing verification. `<` comparisons alongside `||`
also exposed parser ambiguity, avoided with inclusive ranges. These are investigation
candidates, not fixes; independently reduce them against ordinary .NET metadata before
extracting general changes. No neoCLR branch merge into main is appropriate.

The sample's generated async pipeline is transitional: public Task/Result contracts
remain separate from state-machine machinery pending future runtime suspension.
Validation commands and exact limitations live in neoCLR's HTTP experiment README.

The HTTP probe also exposed request interpolation producing no usable value against
this core surface; explicit binary String.Concat construction works. The importer
places a Boolean argument coercion adapter outside its private callee's scope; an
integer flag in the private header helper avoids that adapter. These remain reduced
investigation candidates, not compiler fixes or relaxed runtime access checks.
The public handler still returns Task/Result while the private adapter may be replaced
by runtime suspension later. Generated API documentation covers all seven public types;
internal parser/exchange helpers do not appear as application APIs.

Local macOS validation passes 18 controlled-peer client cases plus Python's HTTP
server with zero retained managed objects in every run, and the .NET 10 baseline.
Private field access checks pass. No Raven code change or main-branch integration
is part of this slice.

### neoCLR HTTP server integration — 2026-09-24

HttpServer adds Listen/GetLocalPort/ServeOne/Close; HttpRequest exposes received
Headers. The neoCLR reference/importer binds those exact contracts while private
request parsing, response encoding and completion adapters remain hidden. The
combined HTTP bootstrap group includes both source files. Match reference, bridge
and library snapshots. Runtime Contract configuration, compiler semantics, generated
state-machine ABI and runtime instructions are unchanged; no Raven code fix is claimed.

The bounded server owns one accepted connection through callback completion and
sending; parse/handler/transfer Result failures close it. Cancellation/runtime Fault
behavior is not translated into HTTP errors. neoCLR's server verifier checks a
separate neoCLR client, independent .NET client, fragmentation and rejection cases
with collection and zero final live managed objects. Exact commands and limits are
in neoCLR's docs/experiments/http-server/README.md.

### neoCLR shared HTTP deadline bridge — 2026-09-24

The socket-backed HTTP exchange now passes one private monotonic deadline through
DNS, address fallback and socket transfers. New native Until submissions and deadline
stamp helpers require matching native runtime, reference, bridge and generated library.
Normal reference Until methods are internal. Bootstrap-only references expose four
cross-slice methods to compile separate library fragments; import restores their
internal contract before validation. The signature matcher permits assembly methods
only when the socket catalog explicitly requests library mode. Application imports
remain rejected; `Probe --network-budget-checks` covers both profiles and the default
signature guard. No Raven compiler code, Runtime Contract setting or state-machine
ABI changed, and no neoCLR-specific change is integrated into Raven main.

The fixed 15-second budget spans lookup through buffered response completion and
retains shorter five-second phases. Custom handler work outside transport and server
application callbacks remain unbounded. The adapter consumes outcomes and closes a
late successful socket before completing an expired request. Matching core metadata
also keeps all public HTTP APIs available in the on-site reference. See neoCLR's
HTTP design and verifier for exact behavior and focused validation.

### neoCLR managed URI reference slice — 2026-09-24

The neoCLR bridge now catalogs System.Uri and UriError. Uri is a managed immutable
class using the existing Equatable/Object hierarchy and Result/error carriers.
Explicit catalog entries admit Uri-to-Object and Equatable<Uri> conversions and
collection/array reference elements. The first application probe caught the missing
Object conversion entry before integration; this was a neoCLR catalog omission,
not a Raven compiler or .NET-target semantic change. No Runtime Contract setting,
compiler emission or runtime native service changed. Matching reference, bridge
and generated library are required. The neoCLR URI probe covers both resolution
overloads, RFC examples, invalid grammar, virtual Object behavior and GC cleanup.
Lexical identity, escaped ASCII and bounded parsing are provisional library policy;
HttpError and BaseUri request integration follow separately.

### Standard-syntax union investigation — 2026-09-24

The neoCLR author directs normal Raven union declarations, including members, as
the class-library default; manually implemented carriers should be rare documented
exceptions. neoCLR's `docs/experiments/http-error-unions` compiles a reduced union
with string and SocketError payloads, a property and an authored ToString override
using the matching installed SDK and existing Runtime Contract settings. No compiler
source, emission policy or Runtime Contract configuration changes were made.

The observed CLI carrier is a sequential value type with a byte tag and typed case
fields. Constructors initialize the receiver with initobj; TryGetValue writes its
output only on the matching branch. An IUnion interface is synthesized in the
application. This differs from the current language spec description of Value as
the only instance storage; reconcile that documentation against general compiler
behavior independently rather than treating this probe as a new permanent ABI.

The baseline neoCLR bridge rejects SocketError byref. A temporary admission exposed
its non-local initobj restriction and was reverted. The reproducible probe checks
the baseline rejection explicitly; it does not establish runtime or GC correctness.
Next work must cover initialization, extraction, default/inactive payloads, copies,
boxing and managed-reference tracing. Public HttpError/BaseUri remain pending, and
no .NET binary compatibility requirement or general compiler fix is claimed.

Follow-up neoCLR bridge work admits constructor receiver initialization and known
error byrefs. Core-UnionAttribute TryGetValue methods with one nested value-case out
parameter map to the existing conditional-output contract at declarations and call
sites; ordinary out methods retain their assignment requirement. The nested
standard-syntax application probe now executes defaults, cases, copies and boxing
under collection pressure. Mixed legacy SocketError nesting remains rejected by
runtime verification because erased System.Value has no managed default. This is
application importer support, not runtime-library migration. No Raven source or
Runtime Contract setting changed; rebuild the matching bridge. HttpError/BaseUri
remain pending.

Validation: the focused probe reports 101 allocations, two collections and zero
live objects; malformed receiver initialization, ordinary-output nonassignment and
conditional-output false success are rejected. Existing constructor-argument checks
pass. The broader record suite, attempted with the installed bundle, fails before
import on Equatable conversions and ambiguous Equals overloads. That is an open
compiler/SDK validation gap, not a passing regression run or a claimed compiler fix.

The next bridge slice admits Raven's explicit-layout empty-case-only union shape:
a core-marked sealed value carrier, one private byte tag at offset zero, and empty
nested case slots separated from the tag. Because those cases have no payload data,
the bridge preserves their field semantics without importing native overlapping
storage. Unmarked layouts, cases with fields and tag overlap remain rejected. This
is target-specific admission, not a change to Raven's CLI layout or Runtime Contract.
The union probe also consumes a separately compiled dependency; runtime-library
reference catalogs/bootstrap exports and mixed erased carriers still need integration.

Validation of the extended fixture passes in single-assembly and separate-library
forms: each reports 103 allocations, two collections and zero live objects. All six
malformed-contract checks and the legacy SocketError default rejection pass.


### Empty-case union bootstrap fragment — 2026-09-24

neoCLR now has a bounded `--library-implementation` path for a standard-syntax
empty-case union and its nested cases/IUnion protocol. A separate core reference
contract must match fields, cases, signatures, output modes, properties and interface
mappings. The test reference contains throwing bodies; imported source bodies execute.
This is a fixture reference generator, not yet the production API metadata pipeline.

Native constructor capabilities disallow replacing the receiver wholesale, so this
bridge translates receiver initobj to checked field-default writes. Empty cases need
no writes. Conditional extraction stays `out(true)` and generated static helper names
use the normal metadata encoding at declarations and call sites. No compiler source,
Raven Runtime Contract option, native opcode or general default-value policy changes.

The bootstrap verifier exercises cases, defaults, computed members, boxed-copy display
and Value extraction; all three allocations are reclaimed. Five mismatched references
are rejected without an implementation artifact. Existing instance-library tests also
pass, including private `var` storage and five rejected contracts. Production core
reference catalogs, consumer binding and public error-type migration remain open.
The author's clarification is recorded in neoCLR conventions: private storage var/val
emit fields; explicit field syntax is intentional or compatibility-oriented.


### Raven-owned union metadata boundary — 2026-09-24

The neoCLR bridge bootstrap fixture now preserves `RavenUnionCaseAttribute` records
(case metadata name, logical name and ordinal) in its projected CLI reference and
validates their match before importing bodies. A separate Raven consumer compiles
construction and matching against that reference. Eight altered contracts are rejected;
the runtime harness still reclaims all three allocations.

A metadata-only generic producer/consumer probe also verifies
`RavenUnionCompanionAttribute` links the case container to the generic carrier,
rejecting missing and wrong targets. This is not generic-union runtime validation.
These are Raven-owned compiler conventions consumed by a provisional bridge adapter,
not neoCLR runtime dependencies or a newly standardized platform case-map contract.
Native output is checked to contain no Raven compiler-services dependency. The
reference fixture preserves selected metadata rather than claiming full custom-attribute
fidelity. Compiler emission, semantics and Runtime Contract configuration are unchanged;
rebuild the matching neoCLR bridge for the new checks. Production core ownership and
mixed legacy carriers remain next work toward HttpError/BaseUri, not an invitation to
expand the generic companion work before the HTTP integration.


### Shared core union protocol import — 2026-09-24

The neoCLR bridge now resolves a standard union's IUnion through its implemented
interface reference. It accepts either the source-synthesized bootstrap interface or
the exact interface in the supplied core reference, validating its public abstract
Object-returning getter. A core-owned interface is mapped without being emitted again
in the union fragment; boxed application calls use ordinary interface dispatch.

The bootstrap verifier recompiles its union against the projected core and exercises
both ownership arrangements. A malformed protocol is a ninth rejected reference
contract. This remains a bridge/fixture change: production reference packaging and
SocketError migration are pending. There is no new Raven compiler emission policy or
Runtime Contract setting, and no Raven metadata dependency in the VM. Rebuild the
matching bridge; neither the public core reference snapshot nor SDK is changed here.

The installed SDK rejects direct source-union-to-IUnion assignment with RAV1504.
The focused application probe therefore boxes to Object and explicitly casts the
reference to IUnion. The implicit conversion is an open compiler candidate; no compiler
fix is claimed or mixed into this bridge change.


### Existing union reference replacement — 2026-09-24

neoCLR's bridge now provides `--project-union-reference SOURCE CORE OWNER OUTPUT`
for the bounded nongeneric empty-case Raven union shape. Existing carrier and nested
case definitions are updated in place so core signatures retain their type identities;
changed case sets are rejected. Reprojection reuses core-owned IUnion and case-attribute
definitions. Reference stubs throw; native bodies continue to come from the source.

The focused SocketError probe covers its thirteen cases, separate consumers of
construction/matching and Socket.Connect's Task/Result signature, recompilation against
the projected core and native library import. The bridge distinguishes this validated
standard shape from its legacy erased-error initialization/receiver catalog.
No Raven compiler source, semantic/emission policy or Runtime Contract option changes.
Rebuild the matching bridge. SDK packaging, runtime callers and the public SocketError
API snapshot are not migrated by this projection check; generic/payload-bearing
projection is unsupported. Raven case metadata stays at the compiler boundary.


### Integrated SocketError source union — 2026-09-24

neoCLR now authors SocketError with normal union syntax. Both core-generation paths
compile its embedded source through the bridge's Raven compiler and project the shape;
a packaged bridge needs neither a checkout nor a separate reference-shape binary.
The native library supplies a shared ordinary IUnion. Runtime call adapters admit the
validated generated case members and conditional outputs, with inactive defaults
instead of erased System.Value storage. Is*/Get* helpers are removed; consumers use
case patterns. Rebuild references, library and applications together. No Raven source
change, emission policy or Runtime Contract option is introduced here.

The nested SocketError/source-HttpError test passes copying, boxing and default checks
and all 13 ToString names using unqualified match arms, with 127 allocations, three
collections and zero live objects. The TCP client, distinct
neoCLR listener/client and selected HTTP success/timeout cases pass. Public API metadata
and documentation are refreshed. The author's next priority is batch migration of
applicable existing unions before resuming HTTP; Is* properties are not a requirement
for union recognition, including Option and Result. Generic class-library projection
and the previously recorded implicit IUnion conversion gap remain open.


### DNS and URI empty-case migration batch — 2026-09-24

neoCLR projects DnsError and UriError from normal union source as well. Compilation
is sequential against the preceding projected reference so Raven reuses the
supplied core's IUnion; compiling all families against a raw seed would require
mapping a source-owned support identity during later projections. No compiler
source, emission policy or Runtime Contract option changes are made. Consumers
must rebuild matching artifacts and replace Is*/Get* calls with case patterns.
The default carriers are inactive, with HasValue false and Value null.

The payload experiment distinguishes mixed managed case storage from all-value
payload explicit layout: the latter still fails closed in the neoCLR importer.
Generic class-library reference projection also remains open. Neither limitation
is resolved by migrating these empty-case families.

Validation: all 23 cases, defaults, boxing and nested managed payloads pass with
147 allocations, three collections and zero live objects. DNS/TCP and URI grammar,
resolution and the recorded .NET comparison pass; the explicit-layout rejection
is separately tested. API snapshot and combined website checks pass.


### Remaining empty-case errors and EntryKind — 2026-09-25

neoCLR now uses standard declarations for StreamError, TextReadError,
StorageLookupError, FileReadError, FileWriteError, ConsoleReadError, Utf8SliceError,
Int32ParseError, IntegerDivisionError and SingleError. Their per-case Is*/Get*
helpers are removed; rebuild matching artifacts and use patterns. Default values
are inactive. Tests of genuinely uninitialized locals disable CLI local initialization.
EntryKind instead uses ordinary CLI enum metadata, File = 1 and Directory = 2,
with zero unnamed. This follows the author's named-constant/variant distinction.

The bridge resolves supplied-core value-case definitions for isinst tokens that
omit a value-type signature flag. No foreign type is admitted by name alone.
No Raven compiler source, emission policy or Runtime Contract option changes.
All 76 error cases, defaults and boxed copies pass (254 allocations, five collections,
zero live objects); 125 scalar outcomes and Console stream/propagation checks pass.
Ten error-library admission checks pass, including malformed shape/case rejection.
The larger storage fixture exceeds the default CLI instruction budget with generated
carrier initialization; its trusted test runner has an explicit larger bound.

Generic Option/Result/TaskOutcome remain handwritten: companion/generic reference
projection and generated payload-body import are not admitted. The separate generic
metadata/consumer test passes and explicitly rejects library projection. Payload
explicit layouts remain rejected. These are tracked bridge work, not permanent
manual-code exceptions or a requirement for per-case Is* members. System.Enum
helpers (TypeInfo and generic overloads) and boxed formatting are the next requested
slice; existing TypeInfo name queries alone do not satisfy that request.


### 2026-09-25: Enum helper target integration

neoCLR's development reference now declares System.Enum.GetNames/GetValues with
both TypeInfo and constrained generic overloads (`where TEnum : struct, Enum`).
The target adapter preserves typed Sequence<TEnum> values while discovery returns
Sequence<Object> boxes of the exact enum type. It validates the generic constraints,
reference/definition signatures and currently admitted Int32 enum definitions.
Collection admission covers enum and Object elements; enum type tokens may resolve
through the supplied core definition when Cecil's IsValueType flag is absent.

The runtime shares unsigned ordering and alias metadata between names, values and
formatting, and target enum lowering supplies a by-reference Object.ToString override.
Current target scope is BindingFlags, TaskState and EntryKind. General application
enum import, other underlying widths and a public flags-helper surface remain outside
this slice. No Raven emission policy or Runtime Contract setting changes are needed;
these are neoCLR bridge/runtime changes, not a general Raven compiler fix.

Validation uses neoCLR's `docs/experiments/enum-helpers` SDK sample, non-enum generic
compile failures, runtime TypeInfo rejection, signature mutations and native metadata,
boxing/heap-limit tests. See the neoCLR record for final run outcomes.

The Enum SDK run passed names/typed values/discovery/formatting under GC pressure
(501 allocations, ten collections, zero live objects); compiler and runtime reject
non-enum inputs. Signature mutation checks passed. The historical Neo bootstrap
keeps explicit legacy carrier and BindingFlags snapshots; it cannot import the
Raven Object/IUnion bodies without the Raven profile. The author puts additional
constants/flags APIs on hold and returns priority to HTTP after this slice.

### 2026-09-25: Nongeneric payload-library projection

The neoCLR bridge now projects matched nongeneric sequential payload union families
from normal Raven declarations. A provisional HttpError family nests core UriError,
DnsError and SocketError plus a string payload. Separate consumer compilation and
execution of imported bodies pass; the execution probe is native IL, not yet an
integrated Raven HttpClient consumer. Generic companion projection and nonempty
overlapping explicit layouts remain rejected. Raven case/companion attributes stay
at the development bridge boundary; runtime output has no Raven metadata dependency.

A generated case formatter exposed an adapter access issue: converting an argument
through a free-standing wrapper lost permission to call the declaring type's private
method. The bridge keeps supported single-materialized-argument conversions at the
original checked call site. It does not broaden private visibility; unsupported
nonpublic multiargument/null conversions still reject explicitly. No compiler source,
emission policy or Runtime Contract configuration changed.

The payload probe passes extraction, inactive default, copies and boxing under GC
(101 allocations, four collections, zero live objects). Private external access,
changed payload contracts and overlapping payload layouts reject. Generic metadata
rejection checks pass, clean bootstrap regeneration matches, and the combined API
website validates 862 pages. Public HTTP typed errors and token-aware Send remain
integration work; this checkpoint only establishes the library bridge prerequisite.

### 2026-09-25: Public typed HTTP results

neoCLR now projects HttpError from normal union source into its compiler reference
and imports its managed bodies. Client, handler, request factory and server results
carry this union; nested resolver/socket values remain inspectable. The new bounded
consumer binding admits only the selected supplied-core family and matched public
signatures. It retains conditional case extraction and rejects forged payload/result
signatures and calls to the compiler-generated private formatter. Constructors now
preserve their newobj instruction through the generic constructor path instead of
being emitted as calls to nonexistent standalone functions.

This is a target bridge/library change: Raven compiler source, semantic model,
emission policy and Runtime Contract options are unchanged. SDK users must rebuild
against matching core metadata, imported library and bridge. Content.ReadText retains
its separate string decoding error; cancellation tokens and BaseUri are later slices.

The client sample exposed the previously tracked async hoisted-local initialization
limitation when naming the intermediate awaited Result. Immediate MapError composition
passes. Interpolating a payload union directly in the server reporting callback did
not produce the expected line; explicitly calling ToString and concatenating does.
Neither observation is claimed fixed. Reduce these independently on ordinary CLI
metadata before considering a general Raven change; do not merge neoCLR policies
into Raven main. Client handler/cause/copy/boxing/GC tests, transport timeout/framing
checks and the JSON application pass. Server invalid-request/response and timeout paths also pass with no live managed
objects after completion. Clean generated-library and API reference checks pass.
neoCLR now selects relevant per-slice tests and skips website builds by author
direction; its site build had already completed for this checkpoint.


## Closed address hierarchy constructor checkpoint — 2026-09-25

The author selects a closed IPAddress class hierarchy instead of the previously
planned value union. An isolated neoCLR probe compiles with current Raven but
exposed a target importer gap for protected base constructors. The bridge now admits
a direct `call` from a derived constructor to its immediate base's protected
constructor. Private constructors and unrelated protected calls remain excluded;
this is not a general protected-member admission change. No Raven compiler source,
Runtime Contract setting or emission policy changes. The address probe checks
immutable copied data, root/Object value equality, hash agreement and GC rooting.
Public IPAddress projection, parsing, formatting and DNS/socket integration remain
pending. This is a target integration checkpoint, not a general Raven fix.


## Public address hierarchy and DNS migration — 2026-09-25

neoCLR integrates a closed IPAddress root with sealed IPv4Address/IPv6Address
implementations, typed parse errors and ordinary managed parsing/formatting. DNS
returns Sequence<IPAddress>; Socket adds value overloads alongside strings. The core
projects the permitted hierarchy and the bridge validates its root/leaves/signatures
and rejects external metadata branches. Source/reference/library/callers must be
rebuilt together. No Raven source, Runtime Contract or emission policy changes.

The focused prototype observed a MetadataLoadContext crash for a private helper
returning Result<byte[], string>; filling caller-owned temporary storage avoids it.
This remains a reduced target observation requiring independent general validation,
not a compiler fix. Byte.ToString selected an Object path that faulted in neoCLR;
numeric formatting widens octets to int. Treat that as a separate runtime/library
investigation, not evidence of a general Raven defect. Typed DNS/echo and independent
HTTP-server checks pass; API and generated snapshots are kept current.


## neoCLR cancellation foundation integration — 2026-09-25

The neoCLR bridge imports System.Concurrency cancellation source/token/registration
contracts. CancellationToken is a sequential struct with exactly one private source
reference; the bridge compares that layout against the selected reference assembly.
Captured and array token addresses are admitted as non-constructor token receivers.
Internal callback-list helpers remain unavailable to application code. No Runtime
Contract setting or Raven semantic/emission rule changes. Optional callback storage
uses Option<Func<Void>>; neoCLR nullable delegate defaults remain unsupported.

The focused neoCLR sample checks source/token copying and boxing under GC, callback
ordering/reentrancy/disposal, and request versus operation acknowledgement. It passed
with 448 allocations, nine collections and zero live allocations at teardown.
Reference/API snapshots and .NET shared-behavior comparison accompany the target slice.
This target admission is not a general compiler change or a candidate for Raven main.

A separate unresolved compiler candidate: a lambda created inside a churn loop that
captures an outer token array failed emission with “Missing local builder for 'tokens'”.
The successful fixture retains the array outside that callback and captures a scalar
token elsewhere. Reduce independently against ordinary .NET before proposing a fix;
no outcome for such a reduction is claimed here.


## neoCLR HTTP address overload integration — 2026-09-25

The neoCLR reference/bridge now expose HttpClient.BaseUri as Option<string>, and
Get(Uri) alongside Get(string) on HttpClient and HttpRequest. These are ordinary
property and overload signatures using the existing URI and union contracts. The
bridge checks the optional-string setter and each overload; no Runtime Contract
setting, compiler semantic-model rule or emission policy changes.

The managed implementation validates/resolves before dispatching to the existing
handler interface. Invalid URI text now preserves UriError in HttpError.InvalidUri.
Base configuration changes affect subsequent construction only; Send consumes a
preconstructed request. Focused target checks cover both overloads, rejection before
handler dispatch, 27 GC collections with zero live allocations, and independent
Python HTTP interoperability. A .NET 10 URI comparison records the deliberate
base-authority and percent-encoded-dot differences. API reference and generated
library snapshots are refreshed; no compiler or website build is required for
this documentation-only Raven update. HTTP cancellation wiring remains separate.


## neoCLR private native cancellation hooks — 2026-09-25

The bootstrap RuntimeServices catalog now admits SocketCancel(Int64) and
DnsCancel(Int64), returning Boolean. The normal application core reference omits
RuntimeServices and UnionImport only enables the catalog for runtime-library builds.
These are target-specific provider hooks, not public Raven APIs or a new Runtime
Contract setting. They use ordinary static call metadata; compiler semantics and
emission are unchanged. No compiler source changes or Raven tests are required.

The native owner now cancels pending connect/accept as well as transfers, keeps the
callback and operation slot until acknowledgement, and preserves committed outcomes.
DNS cancellation retains capacity charged to blocked host work. Eighteen existing/new
cancellation-name Rust tests and the exact native signature/service test passed.
Managed token wiring remains a separate integration step; these hooks alone do not
make HttpClient cancellable. The generated library and API fingerprints are refreshed.

## neoCLR managed networking cancellation — 2026-09-25

Development DNS and Socket operations now have CancellationToken overloads; the
internal shared-deadline paths also accept tokens. The bridge admits the exact
selected signatures and preserves provider/internal access restrictions. Each
provider pre-checks cancellation, registers only admitted native operation IDs,
disposes registration before result consumption, and acknowledges a winning native
cancellation through Promise.Cancel. Native-ready results win over later requests.
HTTP token forwarding remains next. Runtime Contract settings, compiler semantics
and emission policy are unchanged; this is a target library/bridge integration.

The focused neoCLR network-cancellation fixture uses callback stages to isolate
provider ownership from compiler issues. During fixture construction, generic helper
emission hit a MetadataLoadContext mismatch, overloaded captured helpers collided,
Task<Void> awaits left a Void stack value, and pattern-bound values crossing async
or nested-callback boundaries produced invalid/null receivers. A diagnostic variant
also hit an uninitialized state-machine field. These are observations requiring
independent reduction, not fixed compiler bugs or evidence that all are neoCLR-only.
Track them before broad async application testing. General fixes, if confirmed,
must be extracted independently; no experimental branch merge is implied.

The retained loopback fixture passed with 343 allocations, nine collections and no
live objects at teardown. Exact public token signatures, invalid-token rejection and
internal deadline visibility checks passed. neoCLR library/reference snapshots were
refreshed and validated; no Raven compiler tests or website build were run.

## neoCLR HTTP tokens and text helpers — 2026-09-25

HttpHandler now requires Send(HttpRequest, CancellationToken). HttpClient and
HttpSocketHandler retain tokenless convenience calls; HttpClient adds token-aware
Get and string/Uri GetString overloads. The target bridge validates the token value
parameter and maps the updated interface contract. Existing handler fixtures migrate
together, including async forwarding handlers. Runtime Contract configuration,
compiler semantics and emission policy are unchanged; no Raven compiler source
changes are required. Rebuild library and consumers against the matching reference.

The managed transport forwards tokens to DNS/connect/transfers, observes cancellation
before reading child results and closes its owned connection before cancelling its
Promise. GetString composes through existing Task.Map, preserving cancellation and
HTTP errors, then strictly decodes UTF-8. Status support remains 200-only; invalid
UTF-8 maps to HttpError.Protocol. This is a library policy, not a metadata convention.

Bridge signature checks and non-token rejection passed. The two peer-controlled
cancellation cases passed with zero live objects at teardown, as did selected
fragmented/invalid UTF-8 and independent Python server cases through async handlers.
The existing BaseUri fixture passed after handler migration (27 GC collections,
zero live objects). A .NET 10 comparison checks the shared handler/token/text roles
while documenting neoCLR's narrower status/encoding policy. Full library and API
snapshots are refreshed; website and broad platform matrices are skipped.

## neoCLR final HTTP statuses and propagated conversions — 2026-09-25

The target library now returns final statuses 200–599, exposes the Boolean
HttpResponse.IsSuccessStatusCode getter and projects the Int32 payload of the standard
HttpError.UnsuccessfulStatus union case. GetString uses that case outside 200–299.
Reference bindings and exact-signature rejection checks are updated together. Runtime
Contract configuration and Raven semantic/emission policy are unchanged.

The neoCLR `docs/experiments/http-status` fixture demonstrates `?` using an implicit
extension conversion from HttpError into an application union, and ordinary Object
boxing. RAV1506 reports the extension conversion; both execute successfully under GC.
The independent .NET/raw-peer status comparison and neoCLR server framing checks pass,
with zero live objects at teardown. API and full library snapshots are refreshed.

Fixture authoring observed existing candidates for independent reduction: captured
integer ToString addresses rejected by the target importer; compound field assignment
inside a callback emitted a closure receiver for the outer owner's field; constant
patterns called an Object.Equals overload unavailable in the target reference.
Local copies, owner methods and string equality keep the fixture bounded. No general
compiler fix is claimed. Importer diagnostics now include method/instruction context;
its admission rules are unchanged. Any confirmed general compiler fixes must be reduced
and tested independently before main integration. Website builds remain deferred.

A supplementary existing HTTP cancellation headers check is not green in this run:
its handler/text contract checks completed, but a network response reached TimedOut
before the cancellation control signal. The assertion remains strict and the callback
now reports the competing outcome. Track this separately before release; no compiler
or status-slice regression is established by this observation alone.

## HTTP named statuses and property inspection — 2026-09-25

neoCLR adds System.Web.Http.HttpStatusCode as an ordinary CLI Int32 enum with a
bounded common literal set and unnamed numeric values. HttpResponse.StatusCode and
the HttpError.UnsuccessfulStatus payload now use it; a typed response constructor
joins the retained integer overload. Rebuild reference/library/consumers together.
The target bridge validates the enum's exact literals/layout and admits enum-typed
payloads through existing PayloadUnionBindings. No Raven compiler or Runtime Contract
configuration change is made; this is target metadata/library integration.

The target fixture checks named/unnamed numeric conversion and formatting, a typed
response, propagation to an application error union/Object, and optional response
property inspection. Outer `if let` supplies binding for `Headers: headers`; the
illustrative inner `let` is rejected by RAV1613. Property patterns do not require a
Deconstruct contract, and no positional signature or value equality is introduced.
The author's clarification explicitly does not prescribe patterns as the preferred style.

The author's final clarification is covered explicitly: `let`/`if let` provide the
outer capture binding, while an `is` pattern uses inline `let`. The isolated target
probe executes all three forms successfully (eight allocations, zero live objects).
Enum/payload signature checks, the client/server status fixture and snapshot checks
cover the corresponding target integration. Website builds remain skipped.

Cancellation follow-up: the prior checkpoint's original headers check passes; the
current fixture removes the independent request from the signal's prerequisites.
Current headers and isolated-body checks pass with zero live objects. A body check
under overlapping local work still timed out. Keep wall-clock-sensitive checks serial
and retain the limitation; no runtime timeout/scheduling or Raven policy fix is claimed.
