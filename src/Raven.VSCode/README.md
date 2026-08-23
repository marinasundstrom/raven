# Raven VS Code Extension

This extension wires VS Code to the `Raven.LanguageServer` project via the Language Server Protocol (LSP). It provides document synchronization, diagnostics, completions, and inlay hints for `.rvn` files (with legacy `.rav` compatibility) and is designed to run alongside the Raven workspace. The language server publishes fast syntax diagnostics after edits and keeps previous semantic diagnostics and inlays visible for unchanged ranges while newer snapshot results are pending.

It also adds Raven debug integration: F5 can compile and launch either a single `.rvn` file (or legacy `.rav`) or a Raven `.rvnproj` project by invoking a bundled or prebuilt Raven compiler host and then debugging the emitted DLL with the C# debugger.

`.rvnproj` files are associated to VS Code's `xml` language mode by default, so they get XML/MSBuild colorization in the editor.

## Syntax tree visualizer

The Explorer includes an opt-in **Raven Syntax Tree** view modeled after the
Roslyn Syntax Visualizer. It uses `rvn dev syntax json` as its data source and
updates after edits to the active Raven document.

- Expand nodes to inspect syntax nodes, tokens, leading/trailing trivia,
  property roles, raw kinds, spans, missing elements, and diagnostics.
- Select an item to reveal its span in the corresponding source document.
- Use the tree toolbar to switch between the authored tree and the fully
  macro-expanded tree. Switching to the expanded tree opens the complete,
  read-only expanded document beside the active Raven source.
- Use **Raven: Open Expanded Document** to inspect the complete expanded source
  beside the authored document. Selecting items in the expanded tree navigates
  within that read-only expanded document.

The view is always available from the Explorer's Views menu. Run
**Raven: Show Authored Syntax Tree** or **Raven: Show Expanded Syntax Tree**
from the Command Palette to open Explorer, focus the view, and select its mode.
Showing the expanded tree also opens its expanded document.

Project-backed documents are loaded through their `.rvnproj`, so referenced and
same-project macros participate in expansion. Unsaved editor text is supplied
to the tool as an in-memory-style source override without modifying the file on
disk.

## Prerequisites
- .NET SDK installed and on your `PATH`.
- A built Raven language server (`Raven.LanguageServer.dll`). The extension auto-discovers common locations:
  1. A packaged `server/Raven.LanguageServer.dll` folder inside the extension.
  2. A workspace build output at `src/Raven.LanguageServer/bin/Debug/net10.0/Raven.LanguageServer.dll`.
- Alternatively, point the extension at a Raven SDK directory via `raven.sdkPath`, or set the full path explicitly via `raven.languageServerPath`.
- In a Raven source workspace, the extension can build `src/Raven.LanguageServer/Raven.LanguageServer.csproj` on activation before launching the server. This is opt-in via `raven.autoBuildLanguageServerOnActivate`; normal activation uses an existing workspace or packaged build immediately.

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
- `raven.sdkPath`: optional path to a Raven SDK directory containing bundled tools such as `Raven.LanguageServer.dll`, `rvn.dll`, `rvnc.dll`, and `Raven.Core.dll`. This is the easiest way to test different Raven builds with the same VS Code extension.
- `raven.languageServerPath`: override the resolved server assembly path when the defaults do not apply.
- `raven.autoBuildLanguageServerOnActivate`: opt-in source-development setting that builds `src/Raven.LanguageServer/Raven.LanguageServer.csproj` before activation if the project can be found. It can substantially delay startup and is ignored when `raven.languageServerPath` is set.
- `raven.compilerProjectPath`: optional fallback override used to locate a prebuilt `rvnc.dll` under `src/Raven.Compiler/bin/Debug/<tfm>` when no bundled compiler driver can be found.
- `raven.targetFramework`: optional target framework (for example, `net10.0`) passed to Raven debug compilation.

When the extension launches a language server from a workspace build, it stages that build into an isolated extension-owned directory first, then starts the staged copy with the repository root as its working directory. This avoids file locking on the workspace build outputs while still allowing the language server to discover repo-relative assets such as `Raven.Core.dll`.

Run **Raven: Show Toolchain Information** when verifying a development or
installed setup. The Raven output channel reports whether the active extension
is running from a repository development host or an installed extension, the
source and path of its language server, the discovered installed SDK version
and path, and the `Raven.Sdk` version selected by the nearest `global.json` for
each workspace folder. These are independent selections: an installed
extension normally uses its bundled server, while `dotnet build` follows the
project SDK selection and Raven commands use the discovered or explicitly
configured SDK.

## Debugging Raven code (F5)
1. Open a `.rvn`, `.rvnproj`, or `.rav` file.
2. Press F5 and choose `Raven: Compile and Debug` (or run `Raven: Compile and Debug Active File` from the command palette).
3. Standalone files compile under `.raven-build`; projects use their normal
   `bin/Debug/<tfm>` output. The extension launches `dotnet <output.dll>` under
   the .NET debugger.

A Raven library or component project can be debugged through a separate .NET
startup project. This is useful when a Blazor or ASP.NET host references Raven
code:

```json
{
  "type": "raven",
  "request": "launch",
  "name": "Raven: Debug Blazor host",
  "target": "${workspaceFolder}/app/App.rvnproj",
  "startupProject": "${workspaceFolder}/host/Host.csproj",
  "launchProfile": "http"
}
```

The extension builds the startup project, loads the referenced Raven PDBs, and
applies the selected `Project` profile's environment and application URL. When
that profile enables `launchBrowser`, F5 opens the listening web address.
