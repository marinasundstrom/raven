# Raven VS Code Extension

The Raven VS Code extension wires the editor to the `Raven.LanguageServer` LSP process so `.rav` files can surface diagnostics and completions. It auto-discovers the language server build output and starts it with `dotnet` when the extension activates.

## Prerequisites
- .NET 9 SDK available on your `PATH` so the client can start the language server.
- Node.js 18+ for building and running the extension.
- A built `Raven.LanguageServer.dll` (from `src/Raven.LanguageServer/bin/Debug/net9.0` or a packaged `server/` folder).

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
3. Use the **Run Extension** launch configuration in the Run and Debug view. This starts the extension host and attaches to the language server via `dotnet <path-to-Raven.LanguageServer.dll>`.
4. Open or create a `.rav` file to trigger activation and view diagnostics.

## Configuration
The extension exposes a single setting to control how the server is launched:
- `raven.languageServerPath` (string): Override the resolved `Raven.LanguageServer.dll` path. Use this when working with custom build outputs or packaged bits.

## Packaging
When shipping the extension, include a `server/` directory next to `package.json` that contains `Raven.LanguageServer.dll` and its dependencies. The client searches this directory first before falling back to the workspace build output or the user-provided override.
