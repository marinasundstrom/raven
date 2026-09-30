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
The runner compiles Library.rvn through the adapter to a native dependency, reads its declarations with
`NativeAssemblyDefinition`, and creates a reference-only PE projection. It gives that
projection to Raven's **existing .NET semantic loader**, and asks Raven to bind this program:

```raven
func Offset(value: int) -> int {
    return value + 2
}
func Main() -> int {
    return Offset(MathLibrary.Twice(20))
}
```

The compiler-owned adapter in `src/Raven.CodeAnalysis.NeoClr` consumes public semantic
symbols and operation trees through `NeoClrCompilationEmitter.Emit`. It
maps source top-level functions to native functions and the imported call to the
matching projected read-only dependency definition through `AssemblyBuilder.ImportReference`.
The adapter receives no producer builder graph; it explicitly asserts the fixture
core contract. It emits native format-5 bytes through
`AssemblyBuilder.WriteNativeAssembly`, then neoCLR verifies and executes the actual
output and must report/exit with 42. No PE emission or CLI importer is used for the
application. Unsupported division must produce NEOMETA001; an unresolved imported
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
static classes support required Int32 value parameters and Int32 results, value returns, constants, parameter loads, local/static calls and unchecked
unlifted intrinsic addition/subtraction/multiplication are supported. Named/default/
expanded arguments, references, generics, async, captures, fields, instance classes, statements
other than returns, source attributes/modifiers, checked/lifted operators and structural
types are rejected. Dependency binding is deliberately limited to the single fixture
whose native artifact is compiled from Raven and whose reference projection the runner creates; this is not an arbitrary PE importer.

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
Arithmetic is absent from application symbol lookup. Both original native library
files are explicitly supplied to neoCLR. Both module orders execute to 42; missing
and wrong-revision direct/transitive dependencies must fail native verification.
The validation report retains both dependency/projection hashes. This proves format-5
runtime acceptance, not direct PE/#Neo runtime loading or a native semantic provider.
