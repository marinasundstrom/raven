# Raven VS Code Extension

This extension wires VS Code to the `Raven.LanguageServer` project via the Language Server Protocol (LSP). It provides document synchronization and diagnostics for `.rav` files and is designed to run alongside the Raven workspace.

## Prerequisites
- .NET 9 SDK installed and on your `PATH`.
- A built Raven language server (`Raven.LanguageServer.dll`). The extension auto-discovers common locations:
  1. A packaged `server/Raven.LanguageServer.dll` folder inside the extension.
  2. A workspace build output at `src/Raven.LanguageServer/bin/Debug/net9.0/Raven.LanguageServer.dll`.
- Alternatively, set the full path explicitly via the `raven.languageServerPath` setting.

## Building
Install dependencies and compile the extension output:

```bash
cd src/Raven.VSCode
npm install
npm run compile
```

## Running in VS Code
1. Build the language server (`dotnet build src/Raven.LanguageServer/Raven.LanguageServer.csproj`).
2. Open the repository in VS Code.
3. Launch the "Run Extension" target from the debug view. The client starts the server with `dotnet <path-to-Raven.LanguageServer.dll>`.

## Configuration
- `raven.languageServerPath`: override the resolved server assembly path when the defaults do not apply.
