# Playground architecture

The Raven Playground is a browser-hosted environment for exploring Raven
without installing the compiler. It combines a small Blazor WebAssembly user
interface, Monaco editing, the Raven compiler, and in-browser execution. Source
code remains on the device unless the user explicitly copies a share link.

This is development documentation. It describes how the Playground is built
and maintained and is intentionally excluded from the user-facing DocFX site.

## Project layout

The implementation is split across two projects:

- `src/Raven.Playground` owns the Blazor UI, Monaco integration, examples,
  query-string handling, and static publishing.
- `src/Raven.Playground.Worker` owns the persistent .NET Web Worker bridge and
  the compiler-facing services used for completion, compilation, and program
  execution.

The Playground targets .NET 11. Raven can target both .NET 10 and .NET 11, but
the worker integration uses the .NET 11 `blazorwebworker` architecture rather
than maintaining a separate manual compatibility bridge.

## Runtime boundary

The browser main thread owns rendering, navigation, and Monaco. During initial
render, `PlaygroundWorkerClient` starts one persistent module worker and waits
for its .NET runtime to become ready. The worker remains alive for the browser
session so compiler metadata and the Raven workspace are not rebuilt for every
request.

Calls use a deliberately narrow message contract:

1. The Blazor component sends source text plus a position or operation flag.
2. The worker calls a `[JSExport]` method.
3. Complex results are serialized as JSON because the export boundary is most
   reliable with JavaScript-friendly primitives and strings.
4. The main process deserializes the response and updates Monaco or the result
   panel.

Monaco cancellation is checked after an outstanding completion request
returns, so a result for an obsolete editor position is discarded rather than
shown. The compiler request continues in the worker, where it cannot freeze
typing, scrolling, or rendering.

## Compiler services

`PlaygroundLanguageService` owns a Raven `AdhocWorkspace` with one user
document and a generated prelude document. Updating the source produces a new
workspace snapshot while preserving the long-lived project and its framework
references. Completion uses Raven's public compiler completion surface; the
Playground does not infer symbols or types independently.

Compilation emits an in-memory assembly. A compile-only request returns its
size and diagnostics. A run request loads and invokes that assembly inside the
worker and captures standard output and standard error before returning the
observable result to the UI.

The worker embeds the .NET reference assemblies used to compile Raven source.
`Raven.Core` and `Raven.Macros` are also present in the Blazor boot manifest so
the worker runtime can resolve them when macros are loaded or an emitted
program executes.

### Single-threaded semantic access

The worker moves compiler work off the browser UI thread, but its managed
runtime is single-threaded by default. In that environment a blocking
`SemaphoreSlim.Wait` is unsupported and there are no concurrent managed
threads for the semantic gate to serialize. `SemanticModel` therefore uses an
ambient lease without taking the semaphore when
`RuntimeFeature.IsMultithreadingSupported` is false. Threaded .NET runtimes
retain the existing gate.

WebAssembly threads can be enabled with `WasmEnableThreads`, but they require a
threaded runtime and cross-origin isolation headers for all hosting paths. The
Playground does not currently require those headers because it must remain
deployable as a static site. Revisit this choice if the Playground moves to a
host that can guarantee the required response policy.

## Editor behavior

Monaco is bundled from `Editor/editor.js`. Raven TextMate scopes provide syntax
highlighting. Compiler-backed completion is available through `Ctrl+Space`,
opens for member access after `.`, and is debounced for meaningful identifier
prefixes. Automatic identifier completion is suppressed in comments and
strings.

The Playground intentionally aims for a lightweight editor rather than a full
VS Code experience. New continuous semantic features should use the worker and
must include a browser test proving that the UI remains responsive while the
compiler is busy.

## Examples, snippets, and shared source

The query-string contract has three separate source categories:

- `example=<id>` loads an entry from the visible Playground example catalog.
- `snippet=<id>` loads a vetted documentation-owned companion file without
  adding it to the example picker.
- `source=<base64>` loads inline shared source.

Only bundled example and snippet identifiers may honor `run=true`. Inline
source can be loaded and edited but does not run automatically. The Playground
never accepts a source URL and never fetches arbitrary code named by a query
parameter. Invalid identifiers and invalid encoded source fall back to Hello
World with an explanatory UI message.

Documentation snippets remain beside the documentation that owns them. The
shared `docs/snippets/index.json` file is only a manifest. The Playground build
stages the manifest and companion files under generated `wwwroot/snippets/`
content so both static publishing and standalone `dotnet run` use the same
catalog.

## Build and test workflow

Use the focused inner loop:

```bash
dotnet build src/Raven.Playground/Raven.Playground.csproj --property WarningLevel=0
scripts/test-playground-browser.sh
```

The browser smoke test covers static assets, URL loading and validation,
completion, main-thread responsiveness during completion, compilation,
execution, runtime failures, diagnostics, examples, snippets, themes, and
sharing. It runs against published static output rather than relying only on
the development server.

For manual testing:

```bash
dotnet run --project src/Raven.Playground/Raven.Playground.csproj
```

The build target generates Monaco assets and stages documentation snippets
before Blazor prepares static web assets. Generated `wwwroot/js/` and
`wwwroot/snippets/` content is ignored by Git.

## Future direction

The current workspace has one user source file. A multi-file Playground should
add a tabbed editor and extend the worker request contract with a versioned
project snapshot. Public examples, private documentation snippets, and inline
shared programs should remain separate concepts, and copied links must stay
deterministic and safe to load.
