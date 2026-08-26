# Raven project templates

Install the package and create a project with the standard .NET CLI:

```bash
dotnet new install Raven.Templates@VERSION
dotnet new raven-console -n HelloRaven
```

Replace `VERSION` with the Raven prerelease version to install.

Available short names are `raven-console`, `raven-classlib`, `raven-web`,
`raven-browser`, and `raven-nano`. Console, class-library, and ASP.NET Core
templates default to `net11.0`; the browser template uses the stable `net10.0`
WebAssembly toolchain, and Nano targets `netnano1.0`. Override a desktop target
with the standard option:

```bash
dotnet new raven-web -n RavenWeb --framework net10.0
```

The Web template selects `Raven.Sdk.Web`, which composes the normal .NET Web
SDK with Raven's compiler, packages, and ASP.NET Core implicit imports. Other
desktop templates select the base `Raven.Sdk`.

The browser template is a framework-free .NET WebAssembly application rather
than a Blazor application. Raven's built-in `[JSImport]`/`[JSExport]` source
generator demonstrates a typed JavaScript method call, a managed delegate
callback, and a named Raven method invoked from JavaScript without a C#
companion. A future Raven macro can build on that marshalling contract. Install
the standard WebAssembly build tools before building it:

```bash
dotnet workload install wasm-tools
dotnet new raven-browser -n RavenBrowser
dotnet run --project RavenBrowser/RavenBrowser.rvnproj
```
