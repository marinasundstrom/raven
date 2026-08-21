# Browser WebAssembly applications

Raven can build framework-free browser applications on the .NET WebAssembly
runtime. This application type uses `Microsoft.NET.Sdk.WebAssembly`; it does
not reference Blazor or ASP.NET Core. This article is the canonical guide for
browser applications; the broader browser/WASI roadmap remains in
[WebAssembly targets](webassembly.md).

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
creates the runtime, registers the `raven` import module, and starts the managed
entry point with `runMain()`. The WebAssembly SDK copies the static files and
managed runtime into the browser application bundle.
`dotnet run` serves that bundle locally; `dotnet publish -c Release` produces
the optimized static deployment output.

## JavaScript interoperability

The initial application demonstrates both directions across the boundary.
Raven calls a typed JavaScript method and supplies a Raven lambda as a managed
delegate. JavaScript updates the DOM and invokes the delegate to call Raven
back:

```raven
import System.Runtime.InteropServices.JavaScript.*

partial class BrowserInterop {
    [JSImport("setGreeting", "raven")]
    static partial func SetGreeting(
        message: string,
        [JSMarshalAs<JSType.Function<JSType.String>>] onRendered: Action<string>
    );
}

func Main() {
    match JSHost.GlobalThis.GetPropertyAsJSObject("ravenCallback") {
        null => Console.Error.WriteLine("JavaScript did not provide the callback element.")
        JSObject callbackElement => {
            use callback = callbackElement
            let href = JSHost.GlobalThis.GetPropertyAsString("ravenLocation")

            BrowserInterop.SetGreeting(
                "Hello from Raven WebAssembly at $href",
                message => callback.SetProperty("textContent", message)
            )
        }
    }
}
```

`wwwroot/main.js` registers the imported method before running the managed entry
point:

```javascript
setModuleImports('raven', {
    setGreeting(message, onRendered) {
        document.querySelector('#app').textContent = message;
        onRendered('JavaScript called back into Raven.');
    }
});
```

Raven's built-in JavaScript interop source generator recognizes the same
`[JSImport]` declaration shape used by C# and supplies the partial method body
before binding and emit. The first supported marshalling slice is deliberately
small: static partial methods returning `unit`, with `string` and
`Action<string>` parameters. Unsupported declarations report `RVNJS001`.
`JSHost` and `JSObject` remain available for direct property-oriented interop.

A later interop macro layer can replace or build on this generator. Typed Raven
imports and exports could then expand into the low-level .NET WebAssembly
marshalling contract with declaration-local source mapping, giving applications
a Kotlin-like external-declaration experience without changing the hosting
project. The source generator establishes the usable Raven API and runtime
contract first; it does not make Blazor part of the application model.

That macro layer can also support Raven-native HTML or component DSLs that
lower directly to browser interop instead of routing through Blazor. Blazor can
remain one backend without becoming the required application model.

Host-neutral WebAssembly is a separate future target. A `wasi-wasm` or native
Wasm application should model its host capabilities explicitly and must not
inherit this browser template's DOM and JavaScript assumptions. The
[WebAssembly targets](webassembly.md) design note tracks that distinction,
including a future experiment that hosts a server-shaped WASI application
inside a browser.

See the runnable [`browser-wasm` sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/browser-wasm)
and Microsoft's
[JavaScript interop with a WebAssembly Browser App](https://learn.microsoft.com/aspnet/core/client-side/dotnet-interop/wasm-browser-app)
documentation for the underlying runtime model.
