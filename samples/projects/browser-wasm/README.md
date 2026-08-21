# Raven WebAssembly browser sample

This is a framework-free browser application: Raven compiles to a managed .NET
assembly, the .NET WebAssembly SDK publishes the runtime and application into an
`AppBundle`, and a small JavaScript module starts it. Blazor is not involved.

The sample uses the generator-free `JSHost` and `JSObject` APIs to read
`globalThis.location` and update a DOM element supplied by `wwwroot/main.js`.

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

The low-level API is deliberate for the first sample. A future Raven interop
macro can generate the marshalling stubs behind typed JavaScript imports and
exports, replacing the role played by Roslyn source generators in C# while
leaving this project and hosting model unchanged.
