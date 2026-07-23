# WebAssembly direction

Raven has two independent WebAssembly goals:

1. Host `Raven.CodeAnalysis` in a WebAssembly environment so tools can parse,
   bind, inspect, and emit Raven programs without a server-side compiler.
2. Run Raven-produced programs in a WebAssembly environment.

Neither goal depends on the other. A playground may integrate both, but compiler
hosting and executable targeting have separate compatibility contracts and
tests.

## Compiler hosting

The compiler must not assume that loaded runtime assemblies have physical file
locations or that `TRUSTED_PLATFORM_ASSEMBLIES` is populated. A WebAssembly host
supplies the framework metadata references that define its supported compilation
surface. References may be downloaded as static assets and materialized in the
host's virtual filesystem through `MetadataReference.CreateFromImage`.

Compiler metadata inspection in WebAssembly should use portable ECMA-335
readers instead of runtime assembly-loading APIs when only an assembly identity
or metadata is needed. Runtime reflection features that are unavailable in
WebAssembly should fall back to explicit metadata when doing so preserves
correct compiler behavior.

An initial browser-hosted probe has demonstrated:

- loading `Raven.CodeAnalysis` in .NET WebAssembly;
- parsing Raven source;
- binding a `System.Console.WriteLine` call against explicitly supplied
  framework metadata; and
- emitting a managed console assembly to an in-memory stream.

The reference bundle is a host concern. A future browser host should produce a
versioned manifest rather than discovering SDK installation paths at runtime.
The bundle should begin with a deliberately small API surface and grow according
to playground requirements.

## Executable targeting

Raven currently emits managed CLI assemblies. The first WebAssembly executable
target is therefore the .NET WebAssembly runtime: Raven emits ordinary managed
assemblies, and .NET interprets or compiles their IL for browser or WASI
execution.

An independent browser-hosted probe has demonstrated that a console assembly
produced ahead of time by `rvnc` can be loaded and executed under .NET
WebAssembly without loading the Raven compiler.

Browser and WASI hosts expose different platform APIs. Target profiles should
describe those capabilities explicitly, and unavailable APIs should be handled
through normal target-framework reference surfaces and compiler diagnostics.
Direct emission of native WebAssembly, if pursued, is a separate backend and is
not implied by the managed WebAssembly target.

## Next slices

1. Add a repository-owned compiler-host smoke application with a small,
   versioned framework-reference manifest.
2. Add automated browser coverage for parsing, diagnostics, and in-memory emit.
3. Add a separate executable smoke application and browser/WASI execution
   coverage for an ahead-of-time-produced Raven assembly.
4. Define browser and WASI target profiles, including supported runtime APIs,
   threading, filesystem, networking, and dynamic-loading behavior.
5. Build the playground UI only on top of these independently verified layers.
