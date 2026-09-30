# Independent neoCLR metadata consumer

This opt-in executable is the first bounded compiler-to-runtime integration case.
It is not installed as a `Compilation.Emit` backend and does not change ordinary .NET
or the existing neoCLR CLI target. The metadata API remains a separate project.

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

Top-level block-bodied functions and public static methods in public nongeneric
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
private methods, internal/nonstatic types and nonpublic library globals with source
locations and unchanged failed output. This public-only library slice does not add
visibility metadata or a native symbol provider; console globals remain supported.

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
