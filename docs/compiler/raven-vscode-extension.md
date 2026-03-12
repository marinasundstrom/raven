# Raven VS Code Extension

The Raven VS Code extension wires the editor to the `Raven.LanguageServer` LSP process so `.rav` files can surface diagnostics and completions. It auto-discovers the language server build output and starts it with `dotnet` when the extension activates.

## Prerequisites
- .NET 9 SDK available on your `PATH` so the client can start the language server.
- Node.js 18+ for building and running the extension.
- A built `Raven.LanguageServer.dll` (from `src/Raven.LanguageServer/bin/Debug/net10.0` or a packaged `server/` folder).

## Building the extension
Install dependencies and compile the client bundle from the repository root:

```bash
cd src/Raven.VSCode
npm install
npm run compile
```

The compiled JavaScript emits to `out/` and is referenced by the extension manifest.

## Running inside VS Code
1. Build the language server: `dotnet build src/Raven.LanguageServer/Raven.LanguageServer.csproj`.
2. Open the repository in VS Code.
3. In Run and Debug, launch `Raven: LSP + Extension` to start both the extension host and language server.
4. If needed, you can launch only `Raven VS Code Extension Host` (extension host only).
5. Open or create a `.rav` file to trigger activation and view diagnostics.

## Configuration
The extension exposes settings to control language-server resolution and debug compilation:
- `raven.languageServerPath` (string): Override the resolved `Raven.LanguageServer.dll` path. Use this when working with custom build outputs or packaged bits.
- `raven.compilerProjectPath` (string): Override the `Raven.Compiler.csproj` path used for Raven debug launches. Defaults to `<workspace>/src/Raven.Compiler/Raven.Compiler.csproj`.
- `raven.targetFramework` (string): Optional target framework (for example, `net10.0`) passed to debug compile invocations (`dotnet run --framework` and `Raven.Compiler --framework`).

## F5 compile + debug
The extension contributes a `Raven` debug type:
- `Raven: Compile and Debug` compiles the active `.rav` or `.ravenproj` target using `Raven.Compiler`.
- Build artifacts are emitted to `${workspaceFolder}/.raven-debug`.
- After compile succeeds, the extension starts a `coreclr` debug session with `dotnet <compiled-output.dll>`.

You can start it by pressing F5 in a Raven file, or by running `Raven: Compile and Debug Active File` from the command palette.

## Packaging
When shipping the extension, include a `server/` directory next to `package.json` that contains `Raven.LanguageServer.dll` and its dependencies. The client searches this directory first before falling back to the workspace build output or the user-provided override.
