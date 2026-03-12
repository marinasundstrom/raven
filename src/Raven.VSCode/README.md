# Raven VS Code Extension

This extension wires VS Code to the `Raven.LanguageServer` project via the Language Server Protocol (LSP). It provides document synchronization and diagnostics for `.rav` files and is designed to run alongside the Raven workspace.

It also adds Raven debug integration: F5 can compile and launch either a single `.rav` file or a `.ravenproj` project by invoking `Raven.Compiler` and then debugging the emitted DLL with the C# debugger.

## Prerequisites
- .NET 9 SDK installed and on your `PATH`.
- A built Raven language server (`Raven.LanguageServer.dll`). The extension auto-discovers common locations:
  1. A packaged `server/Raven.LanguageServer.dll` folder inside the extension.
  2. A workspace build output at `src/Raven.LanguageServer/bin/Debug/net10.0/Raven.LanguageServer.dll`.
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
3. In Run and Debug, launch `Raven: LSP + Extension` (recommended) to start both the extension host and the language server.
4. If you only need the extension host process, launch `Raven VS Code Extension Host`.

`code --extensionDevelopmentPath=_my_extension_folder.`

## Configuration
- `raven.languageServerPath`: override the resolved server assembly path when the defaults do not apply.
- `raven.compilerProjectPath`: optional override for the `Raven.Compiler.csproj` path used by Raven debug launch.
- `raven.targetFramework`: optional target framework (for example, `net10.0`) passed to Raven debug compilation.

## Debugging Raven code (F5)
1. Open a `.rav` file or `.ravenproj` file.
2. Press F5 and choose `Raven: Compile and Debug` (or run `Raven: Compile and Debug Active File` from the command palette).
3. The extension compiles into `${workspaceFolder}/.raven-debug` and launches `dotnet <output.dll>` under the debugger.
