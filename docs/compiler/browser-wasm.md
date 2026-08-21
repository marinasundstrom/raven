# Browser WebAssembly applications

Raven can build framework-free browser applications on the .NET WebAssembly
runtime. This application type uses `Microsoft.NET.Sdk.WebAssembly`; it does
not reference Blazor or ASP.NET Core.

Create an application with either scaffold channel:

```bash
rvn init browser --name RavenBrowser

# Or, after installing Raven.Templates:
dotnet new raven-browser --name RavenBrowser
```

Install the standard .NET WebAssembly build tools once, then run the project:

```bash
dotnet workload install wasm-tools
dotnet run --project RavenBrowser/RavenBrowser.rvnproj
```

The generated project composes the Raven compiler SDK with the .NET browser
toolchain:

```xml
<Project Sdk="Raven.Sdk/VERSION;Microsoft.NET.Sdk.WebAssembly">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>RavenBrowser</AssemblyName>
    <OutputType>Exe</OutputType>
    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
  </PropertyGroup>
</Project>
```

`wwwroot/main.js` imports the generated `_framework/dotnet.js` module and
starts the managed entry point with `dotnet.run()`. The WebAssembly SDK copies
the static files and managed runtime into the browser application bundle.
`dotnet run` serves that bundle locally; `dotnet publish -c Release` produces
the optimized static deployment output.

## JavaScript interoperability

The initial application uses the generator-free APIs in
`System.Runtime.InteropServices.JavaScript`. JavaScript places an element and
the current location on `globalThis`; Raven reads those values with `JSHost`
and updates the DOM through `JSObject`:

```raven
import System.Runtime.InteropServices.JavaScript.*

func Main() {
    match JSHost.GlobalThis.GetPropertyAsJSObject("ravenApp") {
        null => Console.Error.WriteLine("JavaScript did not provide the Raven app element.")
        JSObject element => {
            use app = element
            let href = JSHost.GlobalThis.GetPropertyAsString("ravenLocation")
            app.SetProperty("textContent", "Hello from Raven WebAssembly at $href")
        }
    }
}
```

This keeps the first app type independent of Roslyn. C#'s typed `[JSImport]`
and `[JSExport]` methods rely on a Roslyn source generator to create marshalling
stubs, so applying those attributes to Raven methods alone is not sufficient.

The intended Raven follow-up is an interop macro layer. Typed Raven import and
export declarations can expand into the low-level .NET WebAssembly marshalling
contract, giving applications a Kotlin-like external-declaration experience
without changing the hosting project. Until that layer exists, use
`JSHost`/`JSObject` directly or keep richer browser calls in a small JavaScript
module that exchanges values through `globalThis`.

That macro layer can also support Raven-native HTML or component DSLs that
lower directly to browser interop instead of routing through Blazor. Blazor can
remain one backend without becoming the required application model.

Host-neutral WebAssembly is a separate future target. A `wasi-wasm` or native
Wasm application should model its host capabilities explicitly and must not
inherit this browser template's DOM and JavaScript assumptions. The
[WebAssembly targets](webassembly.md) design note tracks that distinction,
including a future experiment that hosts a server-shaped WASI application
inside a browser.

See the runnable [`browser-wasm` sample](../../samples/projects/browser-wasm/README.md)
and Microsoft's
[JavaScript interop with a WebAssembly Browser App](https://learn.microsoft.com/aspnet/core/client-side/dotnet-interop/wasm-browser-app)
documentation for the underlying runtime model.
