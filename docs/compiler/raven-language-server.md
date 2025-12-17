# Raven Language Server

The Raven language server provides Language Server Protocol (LSP) support for `.rav` files so editors can surface diagnostics and completions while you work. It is hosted inside the `Raven.LanguageServer` project and wraps the Raven compiler workspace to keep documents synchronized with the editor.

## Features
- **Text synchronization:** Opens, changes, saves, and closes documents through `TextDocumentSyncHandlerBase`, storing the latest text in the workspace.
- **Diagnostics:** Publishes Raven diagnostics for the current file after each change so clients can highlight errors inline.
- **Completions:** Maps the compiler's completion items into LSP responses with snippet ranges for insertion.
- **Framework references:** Loads reference assemblies from the latest installed .NET targeting pack so compilations can bind to standard library types without additional setup.

## Project layout
The server code lives in `src/Raven.LanguageServer` and boots from `Program.cs`, which wires up dependency injection, logging, and the LSP handlers. Key components include:
- `DocumentStore`: Tracks opened documents inside a `RavenWorkspace`, seeds framework references, and converts compiler diagnostics to LSP diagnostics.
- `RavenTextDocumentSyncHandler`: Handles open/change/save/close notifications and triggers diagnostics publication.
- `CompletionHandler`: Uses the compiler `CompletionService` to answer LSP completion requests.
- `PositionHelper`: Converts between LSP positions/ranges and Raven text spans.

## Prerequisites
- .NET 9 SDK on your `PATH`.
- Reference assemblies available from a .NET targeting pack (the server attempts to resolve the latest installed version automatically).

## Building
Restore dependencies and build the server project from the repository root:

```bash
dotnet restore src/Raven.LanguageServer/Raven.LanguageServer.csproj
dotnet build src/Raven.LanguageServer/Raven.LanguageServer.csproj
```

## Running
The server expects to be launched by an LSP client (for example, the Raven VS Code extension) and communicates over standard input/output. To run it manually for debugging:

```bash
dotnet run --project src/Raven.LanguageServer/Raven.LanguageServer.csproj --
```

The process will wait for LSP messages on stdin and emit responses to stdout.
