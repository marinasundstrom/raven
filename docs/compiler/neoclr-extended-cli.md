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
The native writer remains the default and still rejects extended dependencies.

Explicit Raven namespace/module declarations create a logical-module manifest;
that manifest is not yet admitted by the extended writer. The qualified examples
therefore declare global functions that call namespaced imported functions. Nullable
annotations, parameter defaults/modes/attributes, locals, general instructions,
external interface references and broader source-library output remain gaps.
This slice does not qualify the original executable probe that creates a tuple;
it qualifies Raven-produced forwarding libraries executed by a runtime host.

## Validation

`tools/NeoClrMetadataProbe --extended-cli <core> <tuple-library> <Forward.rvn> <output>`
checks import/parameter metadata, both backend transport arguments, native defaults,
unsupported arithmetic and logical-module diagnostics, and atomic failure.
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
