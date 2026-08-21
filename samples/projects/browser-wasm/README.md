# Raven WebAssembly browser sample

This is a framework-free browser application: Raven compiles to a managed .NET
assembly, the .NET WebAssembly SDK publishes the runtime and application into an
`AppBundle`, and a small JavaScript module starts it. Blazor is not involved.

The sample demonstrates both interop directions. Raven calls the JavaScript
`setGreeting` method, passing a Raven lambda as a managed delegate. JavaScript
updates the DOM and invokes that delegate to call back into Raven. JavaScript
also discovers and calls Raven's named `[JSExport]` method through
`getAssemblyExports`.

The Raven source declares the same `[JSImport]` partial-method and `[JSExport]`
method shapes used by C#. Raven's built-in interop source generator supplies
the low-level .NET marshalling and registration stubs, so the application needs
no C# companion and remains independent of Blazor.

Install the .NET WebAssembly build tools once, then run the project:

```bash
dotnet workload install wasm-tools
dotnet run --project samples/projects/browser-wasm/BrowserWasmSample.rvnproj
```

Open the URL printed by the host. For a deployable static site, publish in
Release mode and host the generated `AppBundle` directory:

```bash
dotnet publish samples/projects/browser-wasm/BrowserWasmSample.rvnproj -c Release
```

The source generator is the first typed interop slice. A future Raven interop
macro can generate or refine the same marshalling boundary behind typed
JavaScript imports and exports while leaving this project and hosting model
unchanged.
