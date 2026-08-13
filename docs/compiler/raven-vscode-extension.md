# Raven VS Code Extension

The Raven VS Code extension wires the editor to the `Raven.LanguageServer` LSP process so `.rvn` files, with legacy `.rav` compatibility, can surface diagnostics, completions, and inlay hints. The language server publishes syntax diagnostics immediately after edits and keeps previous semantic diagnostics and inlays visible for unchanged ranges while newer snapshot results are pending. It auto-discovers the language server build output and starts it with `dotnet` when the extension activates.

The Explorer also contains an opt-in **Raven Syntax Tree** debugging view. It
renders nodes, tokens, trivia, syntax property roles, raw kinds, spans, missing
elements, and diagnostics from the machine-readable `rvn dev syntax json`
output. The toolbar switches between the authored syntax tree and the fully
macro-expanded tree, and can open the complete expanded source in a read-only
virtual Raven document. Tree selections reveal the corresponding authored or
expanded source range.

The view is always available from the Explorer's Views menu. Run
**Raven: Show Authored Syntax Tree** or **Raven: Show Expanded Syntax Tree**
from the Command Palette to open Explorer, focus the view, and select its mode.

## Prerequisites
- .NET SDK available on your `PATH` so the client can start the language server.
- The Raven SDK for build, run, and debug commands. The packaged extension can
  provide editor features from its bundled language server without the SDK.
- Node.js 18+ only when building the extension from source.

## Installing the preview

Download and install the VSIX from the matching GitHub release:

```bash
curl -fLO https://github.com/marinasundstrom/raven/releases/download/v0.1.0-preview.7/raven-vscode.vsix
code --install-extension raven-vscode.vsix --force
```

Restart VS Code after installing. If VS Code cannot discover `rvn`, set
`raven.sdkPath` to the versioned directory printed by `rvn sdk path`.

## Building the extension
Install dependencies and compile the client bundle from the repository root:

```bash
cd src/Raven.VSCode
npm install
npm run compile
```

The production JavaScript bundle emits to `dist/extension.js` and is referenced
by the extension manifest.

## Running inside VS Code
1. Build the language server: `dotnet build src/Raven.LanguageServer/Raven.LanguageServer.csproj`.
2. Open the repository in VS Code.
3. In Run and Debug, launch `Raven: LSP + Extension` to start both the extension host and language server.
4. If needed, you can launch only `Raven VS Code Extension Host` (extension host only).
5. Open or create a `.rvn` file to trigger activation and view diagnostics.

## Configuration
The extension exposes settings to control language-server resolution and debug compilation:
- `raven.sdkPath` (string): Override the Raven SDK root directory. When this is unset, the extension runs `rvn sdk path` to discover an installed SDK. The extension looks in that root for bundled tools such as `Raven.LanguageServer.dll`, `rvn.dll`, `rvnc.dll`, and `Raven.Core.dll`.
- `raven.languageServerPath` (string): Override the resolved `Raven.LanguageServer.dll` path. Use this when working with custom build outputs or packaged bits.
- `raven.autoBuildLanguageServerOnActivate` (boolean): Opt-in source-development setting that builds `src/Raven.LanguageServer/Raven.LanguageServer.csproj` before activation if it can find the project in the current workspace or extension ancestors. It can substantially delay startup and is ignored when `raven.languageServerPath` is set.
- `raven.compilerProjectPath` (string): Override the path used to locate a prebuilt `rvnc.dll` under `src/Raven.Compiler/bin/Debug/<tfm>` when no bundled compiler driver is available.
- `raven.targetFramework` (string): Optional target framework (for example, `net10.0`) passed to debug compile invocations.
- `raven.inlayHints.enabled` (boolean): Master switch for Raven inlay hints.
- `raven.inlayHints.inferredTypes.enabled` (boolean): Show inferred type annotation hints when Raven inlay hints are enabled.
- `raven.inlayHints.names.enabled` (boolean): Show name hints for positional invocation arguments and deconstruction elements when Raven inlay hints are enabled.
- `raven.inlayHints.requestDebounceMilliseconds` (number): Delay inlay requests after document edits so typing can settle before semantic inlay work runs.

When the extension discovers a workspace-built language server, it stages that build into an extension-owned directory before launch. The staged copy runs with the repository root as its working directory so repo-relative assets like `Raven.Core.dll` continue to resolve while the workspace build outputs remain free of language-server file locks.

## F5 compile + debug
The extension contributes a `Raven` debug type:
- `Raven: Compile and Debug` compiles the active `.rvn` file or `.rvnproj` target using the `rvnc` compiler driver. Legacy `.rav` source files remain supported for compatibility.
- Build artifacts are emitted to `${workspaceFolder}/.raven-debug`.
- After compile succeeds, the extension starts a `coreclr` debug session with `dotnet <compiled-output.dll>`.

You can start it by pressing F5 in a Raven file, or by running `Raven: Compile and Debug Active File` from the command palette.

## Running a file or project

**Raven: Run Active File/Project** uses the `rvn` frontend. Selecting a `.rvn`
or legacy `.rav` source that belongs to a Raven project runs its owning project
through `rvn run`; selecting a `.rvnproj` file also runs that project. A source
without an owning project runs as an isolated file-based application. The
command opens an interactive terminal rooted beside the resolved project or
source file so application input and output behave like an ordinary script or
console application.

Language features follow evaluated project membership. A source included by a
project receives that project's semantic context; a source outside project
items receives an isolated file-application context even when a project file
exists elsewhere in the workspace.

## Packaging
`scripts/package-vscode.sh` publishes a framework-dependent language server into a `server/` directory next to `package.json` and creates the VSIX. The compiler remains in the separately installed Raven SDK. The client searches an explicit SDK path first, an SDK reported by `rvn sdk path`, workspace build outputs, the packaged server, and finally any user-provided direct assembly overrides.

When no SDK is discovered, the bundled server still provides editor features
and the extension offers a link to the SDK installation instructions. The
prompt can be permanently dismissed for syntax-only installations.
