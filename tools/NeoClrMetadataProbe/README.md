# Independent neoCLR metadata consumer

This opt-in executable is the first bounded compiler-to-runtime integration case.
It can be selected explicitly as a `Compilation.Emit` backend; ordinary .NET and
the existing neoCLR CLI target remain separate defaults/contracts. The metadata API remains a separate project.

```sh
dotnet run --project tools/NeoClrMetadataProbe \
  -p:NeoClrMetadataProject=/absolute/path/to/neoclr/tools/metadata/NeoCLR.Metadata.Experimental/NeoCLR.Metadata.Experimental.csproj \
  -- /absolute/path/to/neoclr /fresh/output/directory
```

The project reference is explicit; the project is non-packable and not in the default
solution build. It uses the independently built metadata library, never copied source.
The runner compiles Arithmetic.rvn and Library.rvn through the adapter into PE/#Neo
containers. `RuntimeAssemblyContainer.ReadCliProjection` reads their reference-only
CLI declarations; Raven's **existing .NET semantic loader** binds directly against
these same files. The application binds this program:

```raven
func Offset(value: int) -> int {
    return value + 2
}
func Main() -> int {
    return Offset(MathLibrary.Twice(20))
}
```

The compiler-owned adapter in `src/Raven.CodeAnalysis.NeoClr` consumes public semantic
symbols and operation trees through `NeoClrCompilationEmitter.EmitMetadataAssembly`. It
maps source top-level functions to native functions and the imported call to the
matching projected read-only dependency definition through `AssemblyBuilder.ImportReference`.
The adapter receives no producer builder graph; it explicitly asserts the fixture
core contract. The separate API emits native format-5 bytes and embeds them in
required #Neo section 256/schema 2 alongside a CLI reference projection. neoCLR
loads the application and both library PE files, verifies native metadata/bodies,
and must report/exit with 42. No CLI body importer is used. Unsupported division must produce NEOMETA001; an unresolved imported
method must retain a compiler binding error. The runner retains source, outputs and
hash evidence in `validation.json` and refuses to overwrite an existing directory.
It also splits Offset and Main into Helper.rvn/Main.rvn and verifies/runs both
source-tree orders to 42. Rejection in the later helper file preserves its diagnostic
location and leaves the output stream untouched.

## Deliberate limits and next steps

The .NET frontend/runtime contract is the bootstrap for this primitive-only test.
This does not install a native Runtime Contract, native semantic-data loader, or
production ICompilationEmitter. The adapter lives in an optional compiler project; the metadata
library remains independent of Raven symbols and bound nodes.
See the [adapter API](../../docs/compiler/api/neoclr-emission.md) for every public
member, the host snapshot-consistency requirement, source-located diagnostics,
validation-before-write behavior and stream I/O limitations.

Top-level functions and public/internal/private static methods with block or expression bodies in public/internal nongeneric
static classes support required Int32 value parameters and Int32 or Unit results, value returns, constants, parameter loads, local/static calls and unchecked
unlifted intrinsic addition/subtraction/multiplication are supported. Named/default/
expanded arguments, references, generics, async, captures, fields, instance classes, statements
other than returns, source attributes/modifiers, checked/lifted operators and structural
types are rejected. Dependency binding is deliberately limited to the two-library fixture
whose containers are compiled from Raven; this is not an arbitrary PE importer.

The public operation consumer already drove shared compiler fixes for binary operator
facts, invocation receivers and required signature-only parameters. The adapter now has explicit immutable configuration, registered assembly-symbol
bindings and Raven diagnostic results. Next add a native metadata provider through
ISemanticDataLoader and extend coverage from actual source cases.
General shared fixes should be integrated independently of this experimental tool.

## Raven library-to-application case

The producer is now Raven source rather than a hand-built metadata fixture:

```raven
public static class MathLibrary {
    static func Twice() -> int {
        return 7
    }
    static func Twice(value: int) -> int {
        return Multiply(value, 2)
    }
    static func Multiply(value: int, factor: int) -> int {
        return Arithmetic.Multiply(value, factor)
    }
}
```

The adapter receives `OutputKind.DynamicallyLinkedLibrary` and emits no entry point.
The application resolves the one-argument overload through projected native metadata;
the native dependency performs its local helper call. All three application variants
return 42. The runner also checks missing/wrong-revision dependencies and rejects
unsupported nonstatic types with source locations and unchanged failed output.
Later visibility slices preserve public/internal static types, public/internal/private
methods and public/internal assembly functions. A native symbol provider remains pending.

## Transitive runtime graph

Arithmetic.rvn is compiled first:

```raven
public static class Arithmetic {
    static func Multiply(value: int, factor: int) -> int {
        return value * factor
    }
}
```

Library.rvn compiles against its projected native declarations; the application
compiles against Library.rvn's projection only. `NativeAssemblyDefinition.References`
retains ArithmeticDependency as a native implementation dependency, although the outer
reference PE omits it because signatures are primitive-only. The runner asserts that
Arithmetic is absent from application symbol lookup. Both PE/#Neo library
files are explicitly supplied to neoCLR. Both module orders execute to 42; missing
and wrong-revision direct/transitive dependencies must fail native verification.
The validation report retains both dependency/projection hashes. This now proves direct PE/#Neo runtime loading. The execution section now uses
bounded CBOR and the runtime decodes directly to its module model. Compiler-host
emission retains a JSON intermediate. A native compiler semantic provider, indexed
native tables and production target registration remain open.

## First acceptance cases: Hello World and a function call

Append `--hello-only` to the runner arguments to focus on these two cases. Both
compile through `EmitMetadataAssembly`, load in neoCLR, print exactly `Hello World`
and exit zero. The second source is:

```raven
func Greet() -> int {
    System.Console.WriteLine("Hello World")
    return 0
}
func Main() -> int {
    return Greet()
}
```

The first puts WriteLine and return directly in Main. The host explicitly supplies
the registered .NET Console reference in `NeoClrEmitOptions`; without that binding,
console calls are unsupported. Only the string-literal WriteLine overload is mapped
to the bundled native System library. Wrong/unregistered reference bindings, other
overloads and Write are rejected with unchanged output. The full probe runs these
cases as well as the existing arithmetic/dependency checks and records PE hashes.

The schema-2 binary decoder retains native semantic format 5 and all existing bounds,
dependency identities, console behavior and diagnostics. This is a provisional object
encoding, not a finalized indexed metadata layout. Older schema-1-only runtimes reject
new output; the matching runtime retains schema-1 compatibility. Loading performance
is measured separately in neoCLR's `examples/metadata_loading.rs`, not inferred from
these execution results.


Unit/no-result helpers and library methods are included in `--hello-only`. The C#
checks exercise explicit/implicit returns, compiler reimport of a library's CLI void
projection, native Unit and Int32 overload calls, and unchanged output on rejected
calls. Parameterless entry points now accept Int32 or Unit, including empty Unit bodies. See the [Unit contract](../../docs/compiler/neoclr-cli-bridge.md#unit-returning-native-helpers-and-library-methods--2026-09-30).


The full probe also checks namespaced libraries: nested block and file-scoped
namespaces, distinct same-name types, calls between source files, compiler reimport
and native execution in both file orders. Namespace-owned free functions and nested
types are rejected until the native declaration contract supports them.


To also exercise the actual compiler command, build `src/Raven.Compiler` with the same
`NeoClrMetadataProject` property (`-f net10.0 -p:UseRavenCoreReference=false`), then add
`--driver /absolute/path/to/rvnc.dll` to the full probe invocation. See the
[native compiler command](../../docs/compiler/neoclr-cli-bridge.md#opt-in-native-compiler-command--2026-09-30)
for usage, native reference limits and the translated System symbol-loader gap.


Translated System integration: invoke the probe with
`--system-symbols <runtime> <rvnc.dll> <System.neox> <fresh-output>`. It verifies that
Math.Min binds to a metadata-derived native callable view, then runs API and compiler
command output against the same binary System to 42. Full core import is not claimed.
See [scope and reproduction](../../docs/compiler/neoclr-cli-bridge.md#translated-system-callable-import--2026-09-30).


The full and `--hello-only` checks also execute Unit Main directly, through a helper,
and as a public static class method, plus an empty entry. The `--driver` checks compile
an actual Unit Main source file and verify native stdout and exit zero.

Native API and driver calls now share `Compilation.Emit` via explicit
`NeoClrEmissionBackend` selection. Adapter checks compare wrapper/shared-pipeline
artifacts and reject debug/core-rewrite options without modifying either stream.

The full probe also emits one release-mode compilation through normal .NET emission
and the native backend. Both execute the shared linear body plan, print `Shared Hello`
and return 42 through static helper calls. This is body-lowering reuse, not complete
type-builder abstraction or wider native language support.

Known follow-up: the optional System-symbol driver case has also been observed binding
`System.Math` from host CoreLib instead of the selected projection. It rejects with
NEOMETA001, leaving output absent. The direct API case passes. Do not treat historical
driver success as a deterministic import contract; metadata loading is the next separate
slice. See `system-symbol-validation.json` for the latest partial result.

The same-compilation checks also cover Unit functions/static methods, explicit and
implicit returns, and an empty entry point. CLI return signatures must be void; both
runtimes print the same output and native Unit programs exit zero.

The same cases now also exercise the shared Int32/Unit callable signature and declaration
builder contract. The .NET adapter declares existing CLI type methods; the native adapter
retains native type-method or assembly-function ownership. No import changes are involved.

The 2026-10-01 lowered-body probe additionally runs implicit Int32 returns and a named
Unit helper call on both runtimes. The adapter now uses the compiler's lowered bound
body, not an independent source-operation traversal. Full native control flow remains
pending. See the compiler's native target codegen migration plan.

The callable-identity case additionally returns 42 from repeated/forward calls across
same-named assembly functions, type methods and overloads on both runtimes.

The source-plan case inspects native metadata for two assembly-owned functions and three
type-owned methods, retains an empty static type, and runs the same source on both runtimes.

Static-type plans are shared by the .NET and native builders. Existing namespaced/empty
class and callable-owner probes exercise this boundary; generic/nested/instance fallback
is covered by the focused compiler C# tests.

The shared-local case initializes immutable/mutable Int32 slots, calls a helper with its
own local, assigns a new value, and returns 42 on .NET and native runtime loading.

The control-flow consumer combines a backward loop, nested if/else, initialized locals
and Console output. It verifies target indices after native Console expansion and returns
42 on both runtimes. Metadata FlowChecks additionally rejects invalid joins and labels.

The loop-exit consumer combines !/!=/<=/>=, continue and break, returning 42 on both
runtimes through the shared lowered-body path.

The primitive-signature cases cover Boolean parameters/results, mixed arguments and
same-arity overloads on both backends, plus separately compiled native library imports.

Typed Boolean local coverage stores a predicate result, reassigns it and compares it
through the shared path, executing the resulting binary assembly in neoCLR.

Short-circuit cases assert output as well as return values: exactly two helper calls
produce output, with the other three right operands skipped by Boolean conditions.

Statement-call coverage discards primitive results from local and imported helpers,
retains side effects in order, and leaves no-result calls without a spurious pop.

Int64 cases cover native long locals/arithmetic, sign extension, extrema/truncation,
and a separately compiled Int64 callable imported through the CLI projection.

Signed unary cases execute +, - and ~ on Int32/Int64 and check wrapping at both
signed minima in the binary assembly loaded by neoCLR.

The primitive cases also validate the shared emission type contract and independent
.NET/native type mappers used by callable declarations and local slots.

PartialTypeChecks also checks split static declarations in both file orders, an empty
part, cross-part overloads, one projected type and unsupported-member rejection.
Both .NET execution and native binary loading must return 42.

String coverage includes computed Unicode console output, String locals/results,
branch returns and a separately compiled native String overload. The driver also
compiles text helpers and imports a String method from its native library.

Capability checks now require native division rejection to identify the unsupported
logical instruction at its source expression. Successful native cases are preflighted
before metadata builder allocation and still verify/run from binary assemblies.

Declaration admission uses the backend profile too: assembly-function ownership,
static methods and static types retain existing native/CLI projection behavior.

Internal static helpers execute on .NET and neoCLR. Namespace library cases also call
an internal helper through a public facade and reject direct external source access;
visibility survives the temporary CLI reference projection.
A raw API-produced binary bypasses Raven's source checker and is rejected by native
verification with `type access denied`, proving runtime enforcement independently.

Signed division runs on both targets, including Int32/Int64 zero and overflow faults.
Native division is now supported; unsupported shift diagnostics preserve the output contract.

Signed remainder also executes on both backends with dividend-sign cases, wide values,
zero divisors and minimum/-1 faults (the CLR edge result is host-specific).

Int32/Int64 bitwise AND/OR/XOR execute through shared lowering on both targets, including sign bits and values wider than Int32.

Left/signed-right shifts execute with Int32 counts and Int32/Int64 values. Paired
checks use in-range counts; CLI oversized counts remain unspecified, while native
counts retain masking. Unsupported floating conversions now check failure output.


### Static method access — 2026-10-01

The paired shared-body case executes private same-owner and internal cross-owner
helper calls on .NET and neoCLR. The namespace library cases preserve method access
through PE/#Neo references in both source orders. Raven rejects external private and
internal calls; raw API-produced callers independently prove native verifier rejection.
Assembly-function visibility and the temporary CLI symbol-loading bridge are unchanged.


Expression-body coverage executes Int32/Int64/Boolean/String results, implicit widening,
Unit helper/entry calls and console output on both targets. Separate-library and rvnc
cases use arrow bodies; unsupported conversions preserve source spans and output.


Boolean &, | and ^ coverage checks all twelve truth-table cases and printed operand
markers on both runtimes, proving eager left-to-right evaluation. The native runtime
must include `fa25609d` on `codex/extended-cli-metadata`; old runtimes reject Boolean
operands for these instructions. Nullable Boolean and enum operators remain deferred.


Primitive conditional-value coverage checks both branches, nested expressions,
Int32/Int64/Boolean/String joins and an unselected division-by-zero branch. The
paired output proves that only the selected side-effecting branch executes.

Value-block coverage also exercises branch-local storage, outer assignments and
discarded calls before the result. Internal if/loop prefixes are supported; returns/outgoing jumps reject output
without writing. Existing statement-form loops remain supported.

The control-flow case retains an earlier arithmetic operand while a value block runs
a loop with internal break/continue and conditional assignments. Both .NET paths and
the native verifier/runtime must preserve the enclosing expression result.


Function-access coverage compiles a library in both file orders with explicit public/
internal and default-internal assembly functions. A public static facade runs to 42
from a separate consumer; raw metadata callers of the internal function are rejected
by native verification. Direct Raven import of CLI-projected globals remains deferred.

## Class-library emission acceptance — 2026-10-01

The author prioritizes compiling actual Raven runtime-library source before broader
metadata loading, then using a broad consumer to drive missing codegen/metadata.
`NeoClrMetadataProbe --class-library-emission <runtime/raven/src> <fresh-output>`
now records source hashes, exact selected source, diagnostic phase and emitted byte
count. It uses the existing host-core primitive bootstrap; no Runtime Contract or
production target configuration changes. Ordinary .NET emission is unaffected.

The first run attempts unchanged Math, UnicodeScalar and GC files. They stop in
binding because native Result/error/RuntimeServices dependencies are absent. This
is not evidence that their bodies or metadata are supported. Selecting the original
Int32 Min/Max/Sign declarations with their System.Math namespace, excluding unrelated
imports/declarations, binds successfully and stops at NEOMETA001: native function
namespace metadata is missing. All failures leave the output empty. No runtime
execution or completed class-library assembly is claimed. The probe reports current
outcomes rather than asserting that unsupported features must remain unsupported.

Next: preserve namespace identity for native ownerless functions and define its CLI
projection explicitly, then emit and execute the selected real Math declarations.
Use neoCLR's order-collections application as the broader acceptance case: it spans
constructors/properties, generic collections/interfaces, arrays/iteration,
lambdas/delegates, Option/Result/patterns and shared reference identity. Compile its
actual library dependency sources as coverage grows; do not substitute fake library
contracts. JSON is a complementary UTF-8/inheritance case; neither sample covers
all language/runtime features. Metadata importer expansion remains deferred.

## Namespaced functions and real Math source — 2026-10-01

Raven now admits block/file namespace functions through a distinct shared target
capability, preserving the full semantic namespace and simple name. Both bounded
backend profiles opt in; ordinary .NET remains the default. Native functions retain
no type owner. No Runtime Contract setting changes. The native adapter requires the
independent metadata/runtime namespace slice `e8611966`; its CLI reference projection
uses reversible encoded global names. Direct source import of these projected globals
is still deferred, as is the general native metadata importer.

`NeoClrMetadataProbe --class-library-runtime <runtime/raven/src> <fresh-output> <neoclr>`
selects the original integer Min/Max/Sign declarations and their System.Math namespace.
It excludes unrelated declarations/imports without rewriting function signatures or
bodies. The selected library now emits successfully. A separate Main source exercises
11 boundary cases (Int32 endpoints, equality and all Sign branches), in both file
orders, through ordinary CLI execution and binary native verification/execution to 42.
The probe also asserts exact native System.Math namespace and null owners; it writes
source hashes, selected source and runtime hash. This uses host core primitives and
is not a full System build. Whole Math/UnicodeScalar/GC files still stop in binding
on absent native library dependencies. The order-collections consumer is the next
acceptance expansion; its constructors/properties, generics and delegates exceed the
current static primitive producer.

## Order consumer frontier and shared property identity — 2026-10-01

The broad acceptance seed is neoCLR's
`docs/experiments/raven-target/samples/application-order-collections.rvn`.
`NeoClrMetadataProbe --consumer-emission <sample.rvn> <fresh-output>` now inventories
the unchanged full source and its exact global Order declaration. It records original
and selected hashes, selected source, diagnostic phase/count (first 32 messages),
and actual semantic members. It uses host-core references only; it does not replace
native collection/LINQ/union dependencies with stubs. Full-source binding errors are
not assertions about emission coverage. No Runtime Contract setting or target default
changes, and this inventory does not claim native object execution.

The isolated Order declaration binds with zero errors and reaches the native
nonstatic-class gate. It contains two instance properties, two backing fields, four
accessors and a constructor. Repeated binding exposed a general accessor/backing-field
identity bug, now fixed in the shared member binder and independently validated by
ordinary .NET execution. The producer must consume those canonical symbols, not
filter duplicate names as a backend workaround.

Next implementation sequence: shared nominal type/receiver references and nonstatic
type definitions; primitive instance fields and constructor/accessor method contracts;
property-to-accessor associations; then allocation, constructor calls and instance
field access. Use the selected real Order declaration plus creation/mutation/aliasing
checks on both targets. Preserve ordinary CLI Field/Property/MethodSemantics concepts
where applicable; add explicit native mappings behind target capabilities. Do not
strip source properties into an ad hoc field-only contract. Generic collection and
union/delegate coverage follows that first object case; broad native symbol importing
remains deferred.

## Executable vector milestone — 2026-10-01

The Order frontier above has advanced through constructors, properties, nominal
signatures and arrays. Run:

```sh
dotnet run --project tools/NeoClrMetadataProbe -p:WarningLevel=0 \
  -p:NeoClrMetadataProject=/absolute/path/to/neoclr/tools/metadata/NeoCLR.Metadata.Experimental/NeoCLR.Metadata.Experimental.csproj \
  -- --array-runtime /absolute/path/to/application-order-collections.rvn \
  /tmp/fresh-array-output /absolute/path/to/neoclr/target/release/neoclr
```

This selects the original Order declaration and uses its batch expression in a
separate consumer, preserving host-core bootstrap binding. It checks literal arrays,
primitive and nominal storage, overloads, property projections, alias mutation, ordered
index/value evaluation, Length, empty arrays and nested/labeled for iteration on .NET
and binary neoCLR in both source orders. Expected result is 42. Output contains the
selected sources, native binaries and validation.json with source/runtime SHA-256.
`array-runtime-validation.json` is the checked-in evidence; this does not claim the
full generic collections consumer or System class library compiles. Fixed-length type
contracts, nested/multidimensional arrays, spreads and generic enumerators remain
outside bounded native emission.

## Indexed collection acceptance

`NeoClrMetadataProbe --indexer-runtime <application-order-collections.rvn> <fresh-output> <neoclr>`
compiles the original Order declaration with a separate concrete indexed collection.
It verifies/runs both source orders on .NET and binary neoCLR (42), checks indexed
property projections and alias/evaluation-order behavior, propagates an indexed bounds
fault on both targets, and rejects unsupported signatures without output. The checked-in
indexer-runtime-validation.json records source/runtime hashes. This is not a claim
that generic ArrayList/HashMap or the complete original consumer compiles.


Whole-file runtime acceptance:

```sh
dotnet NeoClrMetadataProbe.dll --whole-library-runtime <neoclr/runtime/raven/src> <fresh-output> <neoclr-executable>
```

Compiles System.Globalization.Language unchanged with a consumer, runs both source
orders on CLI/native, and requires und/sv/he output with exit result 42. Additional
consumer code checks static setter mutation and a constructed generic static getter;
unsupported static storage must reject without output. Validation records hashes and
explicitly marks the host-core bootstrap and incomplete full-library status. The
class-library-emission inventory now also includes Language, Comparer, EqualityComparer
and ArrayList without replacing their native dependencies.


`--interface-library-runtime <source-root> <fresh-output> <runtime>` compiles the
unchanged Comparer, EqualityComparer, Disposable and Iterator files, checks interface metadata on CLI/native
projection and verifies/loads both file orders. Its independent entry returns 42;
this command deliberately makes no interface-dispatch claim.

### Native-profile vector library boundary

```sh
dotnet run --project tools/NeoClrMetadataProbe -p:WarningLevel=0 \
  -p:NeoClrMetadataProject=/absolute/neoclr/tools/metadata/NeoCLR.Metadata.Experimental/NeoCLR.Metadata.Experimental.csproj \
  -- --vector-library-runtime /absolute/neoclr /tmp/fresh-vector-output /absolute/neoclr/target/release/neoclr
```

Emits independent Raven library/application binaries with the neoCLR declaration
core, verifies and executes the pair (42), and records core/runtime/image hashes.
Checks static Int32/Int64/Boolean/String vector overloads, returned array aliasing,
mutation, void calls, iteration and transactional rejection of missing dependency
bindings or incompatible vector overloads. No host core or CLI body importer is used.
Symbols still use projected CLI declarations; nominal/generic imports remain deferred.

### Imported generic library boundary

Use `--generic-library-runtime <neoclr-root> <fresh-output> <neoclr-executable>` with
the same NeoClrMetadataProject build property as the vector probe. The C# driver
emits and runs a separate generic library/application using the native target profile.
It checks primitive/vector substitutions, void calls, aliases and missing bindings or
generic declarations without output writes. Method arity overloads in this Raven
consumer also have different value-parameter counts; same-signature generic-arity
resolution is covered separately by the metadata API tests. See the compiler bridge
doc for the open Raven binding observation. Nominal/generic owners remain unsupported.


The `--generic-collection-contract-runtime <neo-root> <fresh-output> <runtime>` mode
executes generic provider/iterator implementations of the unchanged source collection
contracts on CLR and neoCLR in both source orders (42). The target cases use the matching
CoreProbe Self marker and CompilationOptions.NeoCLR.

The exploratory `--library-source <neo-root> <fresh-output> <implementation-seed.dll>
<System.neox>` mode binds unchanged ArrayList and its interface hierarchy, reports the
native emission boundary, and performs a nonexecuted CLI emission control. Generate the
seed with neoCLR's raven-target probe `--reference-library-core`; the consumer reference
omits bootstrap intrinsics intentionally. The report is not an execution acceptance test.


`--reserved-storage-runtime <implementation-seed.dll> <fresh-output> <runtime>` verifies
explicit bootstrap opt-in, generic reservation, publication and unread-slot faults in
Raven-emitted native PE. The library-source inventory also opts into the same registered
seed; consumer/default native emission does not gain the intrinsic implicitly.

### Executable library-source checkpoint

`--array-list-source-runtime <neo-root> <fresh-output> <implementation-seed.dll> <System.neox> <runtime>`
compiles the unchanged ArrayList source hierarchy with success and failure consumers,
verifies each emitted native PE and checks execution. validation.json records sources,
consumer and seed/System hashes; execution.json records the runtime hash and results.
The implementation seed is not the consumer projection and its stubs must not execute.

`--namespace-function-runtime <seed.dll> <fresh-output> <runtime> <System.neox>`
checks explicitly bound System.Fail calls, including a dynamic message.

`--comparer-source-runtime <neo-root> <fresh-output> <implementation-seed.dll> <System.neox> <runtime>`
compiles unchanged callback comparer sources and executes generic Function fields with
noncapturing callbacks through concrete and interface receivers.

`--hash-map-source-runtime <neo-root> <fresh-output> <implementation-seed.dll> <System.neox> <runtime>`
compiles unchanged HashMap with its source collection/policy dependencies and checks
collisions, growth, duplicate rejection, update/insert, independent key snapshots,
interface dispatch and Option lookups. Success returns 42 after native verification.

The HashMap source checkpoint also checks internal Order reference payloads, mutation
visibility across map/list/filtered-list storage, replacement independence and iterator
reads. Both source consumers return 42.

`--assess-source-application <neo-root> <fresh-output> <implementation-seed.dll> <System.neox>`
records two inventories of the unchanged broad collections application: source-built
collections with translated queries, and source-built collections plus unchanged query
operators. This is an assessment command; successful process exit means reports were
written, not that the application emitted. Inspect each validation.json phase/diagnostics.
The current mixed bootstrap exposes source/seed iterable identity mismatches.

### Direct native semantic input

`--native-symbols <NeoCLR.CoreProbe.dll> <fresh-output>` writes a small native function
library using the metadata API and reads it directly into Raven through
NeoClrMetadataReference.ReadAssembly. It checks overload/argument/accessibility binding,
semantic symbol/type identity, both reference orders, compilation isolation, exact version
matching and dependency/configuration errors. CLI and native emission rejection must leave
output empty until the native call adapter exists. validation.json records scope, hashes
and passed checks. This is semantic import evidence, not native execution evidence.

Native semantic import now also emits API-authored and Raven-authored cross-assembly
calls. To run both consumers against neoCLR and record artifact/runtime hashes:

```sh
dotnet tools/NeoClrMetadataProbe/bin/Debug/net10.0/NeoClrMetadataProbe.dll \
  --native-symbols-runtime /path/to/NeoCLR.CoreProbe.dll /path/to/neoclr \
  /path/to/System.neox /tmp/native-symbol-runtime-fresh
```

Both return Int32(42). The simpler --native-symbols mode emits the same artifacts
without running them. CLI emission still rejects native references. Missing explicit
emission bindings and mismatched native snapshots remain errors; native primitive
namespace functions and a CLI core bootstrap are the tested boundary.

The native-symbol modes also build a Raven static class library, read its native type
and method definitions, and emit a consumer selecting Boolean/Int32 overloads. Runtime
mode runs this third consumer (42). Both input reference orders, canonical type/member
identity, internal type/member and private member access, and argument mismatch are
checked. The admitted type subset is fieldless nongeneric top-level static classes.

NativeTypeConsumer additionally constructs a fieldless Calculator, stores an alias and
calls its primitive Add method. NativeTypeChecks verifies constructor classification
and RAV0500 for private constructors/instance methods. Reference equality/inequality
expressions remain a recorded portable-lowering gap, not part of the passing consumer.

The class consumer now initializes private primitive storage in its constructor and
reads it through an imported instance method (42). Field-symbol ownership/type/access
and private-field RAV0500 are checked. Direct public field emission is a separate
negative check: semantic binding succeeds, but NEOMETA001 must leave output empty.

Direct public primitive field emission is now positive coverage: the main class consumer
stores through an alias and loads through the original reference, and NativeFieldConsumer
executes a constructor followed by a direct field load. Runtime mode runs all four
consumers (42). The earlier NEOMETA001 field-operand limitation is closed for this subset.

The native type consumer now also exercises local nominal signatures: factory results,
namespace/static/instance class identity calls, and a constructor receiving another
class. Semantic checks require the same canonical type symbols in both reference orders;
invalid nominal arguments diagnose. All four native runtime consumers still return 42.
The direct dependency remains PE/#Neo; only the primitive bootstrap core uses CLI metadata.

The native type consumer additionally exercises a class-valued field: replace the object,
mutate the replacement through nested field access, and check original-object independence
before returning 42. Nominal field symbol identity and invalid assignments are checked.
No CLI projection of the native library is involved.

Runtime mode now runs five consumers. ExternalNativeChecks builds NativePayloadLibrary,
then NativeHolderLibrary using the payload's direct native reference, then a consumer
using both. Constructor/method/field signature identity is checked in both reference
orders; missing assemblies, wrong versions, missing types and duplicates diagnose. The
runtime harness supplies both modules, hashes both artifacts and verifies result 42.

ExternalNativeChecks now also covers Payload[] constructor/field/method signatures and
Int32[] methods. The consumer replaces an element through a stored array alias and
passes a primitive array across libraries, returning 42. Field/parameter/result array
symbols share exact external element identity in both reference orders.

ExternalNativeChecks also covers native non-indexed properties: class/array get/set,
read-only and private-set properties, and a static getter. Property signatures reuse
canonical types and accessor symbols in both reference orders. The consumer replaces
objects/arrays via setters, reads through getters and returns 42. Read-only/private
setter assignments must diagnose before emission.

The external consumer now reads/writes an imported Int32 indexer and reads a String
overload. Both return the canonical external Payload type. Tests check index parameter
identity, private-setter/wrong-index diagnostics and indexed replacement through an
array alias. All five runtime cases still return 42.

The Boolean indexer overload has only a setter. The native consumer replaces a Payload
through it and observes the result through another indexer (42). Symbol checks verify
that only the Boolean index appears in Parameters, excluding the setter value; reads
from the setter-only overload diagnose. This shares the ordinary .NET binder path.

Runtime mode now runs six consumers. NativeInterfaceChecks compiles a library with
nongeneric interface inheritance and two root-class implementations. Factories return
the derived interface; the separate consumer calls inherited methods/properties (42).
The direct native symbols preserve interface classification, abstract/virtual flags,
canonical direct/transitive relationships and reference-order independence.
