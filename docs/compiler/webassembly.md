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

The reference bundle is a host concern. `src/Raven.Playground` embeds the
version-pinned .NET browser runtime reference closure at build time rather than
discovering SDK installation paths in the browser. Reducing that closure is a
future payload optimization; correctness currently takes precedence over a
hand-maintained reference shortlist.

## Executable targeting

Raven currently emits managed CLI assemblies. The first WebAssembly executable
target is therefore the .NET WebAssembly runtime: Raven emits ordinary managed
assemblies, and .NET interprets or compiles their IL for browser or WASI
execution.

An independent browser-hosted probe has demonstrated that a console assembly
produced ahead of time by `rvnc` can be loaded and executed under .NET
WebAssembly without loading the Raven compiler.

`src/Raven.Playground` now combines the two independently owned paths in a
static Blazor WebAssembly application:

- Monaco hosts the source editor.
- Raven's existing TextMate grammar supplies the initial lexical highlighting.
- Monaco completion is supplied by a browser-hosted Raven `AdhocWorkspace` that
  advances the document through ordinary immutable solution snapshots and calls
  the public compiler completion API.
- **Compile** diagnoses and emits the workspace's current managed compilation,
  reusing semantic state already established by editor requests when possible.
- **Run** passes that assembly to a separate runner and captures its console
  output.

Compiler-produced semantic highlighting remains a later editor-service layer;
it is not part of the TextMate integration.

Build and publish the playground with:

```bash
dotnet publish src/Raven.Playground/Raven.Playground.csproj \
  -c Release \
  -o artifacts/playground
```

The deployable site is `artifacts/playground/wwwroot`. It can be served by any
static file host. The host must serve `.wasm` files as `application/wasm` and
fall back to `index.html` for client-side routes.

Run the end-to-end browser smoke test with:

```bash
scripts/test-playground-browser.sh
```

The test publishes a release build, serves only its static `wwwroot` output,
and uses headless Chromium to verify Monaco startup, TextMate tokenization,
semantic member completion and insertion, successful compilation, compiler
diagnostics, emitted-assembly loading, and captured program output. Its first
run installs the pinned Playwright Chromium build.

Browser and WASI hosts expose different platform APIs. Target profiles should
describe those capabilities explicitly, and unavailable APIs should be handled
through normal target-framework reference surfaces and compiler diagnostics.
Direct emission of native WebAssembly, if pursued, is a separate backend and is
not implied by the managed WebAssembly target.

## Next slices

1. Define browser and WASI target profiles, including supported runtime APIs,
   threading, filesystem, networking, and dynamic-loading behavior.
2. Add compiler-produced semantic tokens without replacing the TextMate lexical
   fallback.
3. Reduce and cache the framework metadata payload without reintroducing an
   incomplete reference closure.
