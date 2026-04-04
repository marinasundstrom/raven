# Raven VS Code Extension

This extension wires VS Code to the `Raven.LanguageServer` project via the Language Server Protocol (LSP). It provides document synchronization and diagnostics for `.rvn` files (with legacy `.rav` compatibility) and is designed to run alongside the Raven workspace.

It also adds Raven debug integration: F5 can compile and launch either a single `.rvn` file (or legacy `.rav`) or a Raven project file (`.rvnproj` or legacy `.ravenproj`) by invoking a bundled or prebuilt Raven compiler host and then debugging the emitted DLL with the C# debugger.

`.rvnproj` and legacy `.ravenproj` files are associated to VS Code's `xml` language mode by default, so they get XML/MSBuild colorization in the editor.

## Prerequisites
- .NET SDK installed and on your `PATH`.
- A built Raven language server (`Raven.LanguageServer.dll`). The extension auto-discovers common locations:
  1. A packaged `server/Raven.LanguageServer.dll` folder inside the extension.
  2. A workspace build output at `src/Raven.LanguageServer/bin/Debug/net10.0/Raven.LanguageServer.dll`.
- Alternatively, point the extension at a Raven SDK directory via `raven.sdkPath`, or set the full path explicitly via `raven.languageServerPath`.

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
- `raven.sdkPath`: optional path to a Raven SDK directory containing bundled tools such as `Raven.LanguageServer.dll`, `rvn.dll`, and `Raven.Core.dll`. This is the easiest way to test different Raven builds with the same VS Code extension.
- `raven.languageServerPath`: override the resolved server assembly path when the defaults do not apply.
- `raven.compilerProjectPath`: optional fallback override used to locate a prebuilt `rvn.dll` under `src/Raven.Compiler/bin/Debug/<tfm>` when no bundled compiler host can be found.
- `raven.targetFramework`: optional target framework (for example, `net10.0`) passed to Raven debug compilation.

When the extension launches a language server from a workspace build, it stages that build into an isolated extension-owned directory first, then starts the staged copy with the repository root as its working directory. This avoids file locking on the workspace build outputs while still allowing the language server to discover repo-relative assets such as `Raven.Core.dll`.

## Debugging Raven code (F5)
1. Open a `.rvn`, `.rvnproj`, `.rav`, or `.ravenproj` file.
2. Press F5 and choose `Raven: Compile and Debug` (or run `Raven: Compile and Debug Active File` from the command palette).
3. The extension compiles into `${workspaceFolder}/.raven-debug` and launches `dotnet <output.dll>` under the debugger.
