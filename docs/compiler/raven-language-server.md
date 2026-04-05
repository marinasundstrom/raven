# Raven Language Server

The Raven language server provides Language Server Protocol (LSP) support for `.rvn` files, with legacy `.rav` compatibility, so editors can surface diagnostics and completions while you work. It is hosted inside the `Raven.LanguageServer` project and wraps the Raven compiler workspace to keep documents synchronized with the editor.

## Features
- **Text synchronization:** Opens, changes, saves, and closes documents through `TextDocumentSyncHandlerBase`, storing the latest text in the workspace.
- **Diagnostics:** Publishes Raven diagnostics for the current file after each change so clients can highlight errors inline.
- **Completions:** Maps the compiler's completion items into LSP responses with snippet ranges for insertion.
- **Hover symbol projection:** Hover on member-access segments resolves the member symbol for both identifier and access operators (for example `.Name` and `?.Name`), including carrier/conditional-access chains.
- **Hover capture annotations:** Hover on lambdas and nested `func` statements includes captured-symbol lists. Hover on captured locals/parameters marks them as captured variables.
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

## Logging
Raven language-service troubleshooting uses two different log sinks:

- **Server log:** [Program.cs](/Users/robert/Projects/Raven/src/Raven.LanguageServer/Program.cs) resolves the repository root and writes the language server's file log to `logs/raven-lsp.log`.
- **VS Code client log:** [extension.ts](/Users/robert/Projects/Raven/src/Raven.VSCode/src/extension.ts) writes lifecycle, request-start, request-complete, and request-failure lines such as `[lifecycle ...]` to the VS Code `Raven` output channel.

When investigating missing diagnostics, hover cancellations, or completion/definition issues, capture both:

1. `logs/raven-lsp.log` from the repository root.
2. The `Raven` output channel contents from VS Code.

The two logs answer different questions:

- `logs/raven-lsp.log` shows what the server process actually handled, including parser/diagnostics exceptions, code-action failures, and server startup/shutdown.
- The `Raven` output channel shows what the client requested and whether requests were canceled before the server answered.
