# Raven Programming Language

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Docs](https://img.shields.io/badge/docs-available-brightgreen.svg)](docs/)

Raven is an experimental .NET language and compiler. It combines an
expression-oriented surface, explicit mutability, structural pattern matching,
`Option`/`Result`-based flow, and direct interoperability with existing .NET
libraries.

The implementation is also a compiler-as-a-service playground. The compiler
core follows a Roslyn-like shape with immutable syntax trees, semantic models,
diagnostics, and services that can support command-line compilation, editor
features, analyzers, and language experiments.

Raven is source-build oriented today. The long-term distribution goal is to
package the `rvn` frontend, `rvnc` compiler driver, language server, build
assets, and `Raven.Core` together so projects do not depend on repo-relative
paths.

## Start Here

- [Getting Started](docs/getting-started.md) - build the compiler, run a sample,
  and create a small project.
- [Language Introduction](docs/introduction.md) - guided language overview.
- [Language Philosophy](docs/lang/philosophy.md) - design principles for Raven
  language changes.
- [Language Specification](docs/lang/spec/language-specification.md) - current
  normative language docs and grammar links.
- [Compiler Docs](docs/compiler/index.md) - architecture, APIs, diagnostics,
  language server, and development notes.

## Language Snapshot

Raven favors:

- expression-oriented code, while keeping statements for effects and early exits
- plain top-level functions for standalone operations and workflows
- signposted declarations: `func`, `val`, `var`, `event`, `class`, `union`,
  `case`, and related keywords say what is being declared
- explicit immutable and mutable bindings with `val` and `var`
- explicit pattern bindings in `match`, `if`, `while`, `for`, and deconstruction
- `Option<T>` for absence and `Result<T, E>` for expected failure
- records, primary constructors, unions, and target-typed shorthand
- direct use of .NET libraries, async APIs, collections, and IL tooling

Raven has no `void`; the empty result type is `unit`, written as `()`.

```raven
import System.Console.*
import System.Linq.*

func Main() -> () {
    val requests = [
        ShipmentRequest("REQ-1001", "NorthStar", 10),
        ShipmentRequest("REQ-1002", "Oceanic", 3)
    ]

    val message = FindRequest(requests, "REQ-1002") match {
        Ok(val request) => "Ready: ${request.Id} via ${request.Carrier}"
        Error(val err) => "Cannot quote shipment: $err"
    }

    WriteLine(message)
}

func FindRequest(requests: ShipmentRequest[], id: string) -> Result<ShipmentRequest, string> {
    return requests.FirstOrError(r => r.Id == id, () => "request not found")
}

record class ShipmentRequest(val Id: string, val Carrier: string, val WeightKg: int)
```

The example shows ordinary .NET interop (`System.Console`, LINQ-style extension
methods), explicit immutable bindings, records with promoted constructor
parameters, and `Result`-driven recoverable flow.

## If You Are Coming From C#

Raven is not trying to replace the .NET ecosystem around C#. It is exploring a
different source model for common application and compiler problems while still
emitting regular .NET assemblies.

| In many C# codebases | Raven's preferred shape |
| --- | --- |
| Static helper classes used only to hold methods | Plain top-level functions |
| Context-dependent declarations where shape is inferred from placement | Declaration keywords such as `func`, `val`, `var`, `event`, and `union` |
| `null` as absence in domain data | `Option<T>` with `Some(...)` and `None` |
| Exceptions for expected lookup or validation failure | `Result<T, E>` with `Ok(...)` and `Error(...)` |
| `enum` plus nullable detail fields | `union` cases with typed payloads |
| `switch` plus type/null checks spread across methods | `match` expressions over values and patterns |
| Mutable locals by convention unless avoided | `val` by default, `var` when mutation is intended |
| `void` methods | `()` (`unit`) return values |

The [Getting Started](docs/getting-started.md) walkthrough uses a C#-style
shipment quote problem to show these differences in running Raven code.

## Repository Layout

```text
src/
  Raven.CodeAnalysis/         Compiler core: syntax, binding, semantic model, emit
  Raven.Compiler/             rvnc compiler driver
  Raven/                      rvn developer/project command frontend
  Raven.Core/                 Raven core library
  Raven.LanguageServer/       Language server implementation

test/
  Raven.CodeAnalysis.Tests/   Compiler unit tests
  Raven.Core.Tests/           Core library tests

samples/                      Runnable Raven files and project samples
tools/                        Syntax, bound node, operation, and diagnostic generators
docs/                         Language, compiler, and contributor documentation
```

`test/Raven.CodeAnalysis.Samples.Tests` is a legacy sample-test project and is
not part of the normal test focus.

## Prerequisites

- A .NET SDK with `net10.0` targeting support.
- A shell that can run the repository scripts.
- Optional: [DocFX](https://dotnet.github.io/docfx/) for generated API docs.

Some samples target `net11.0`; those samples include or require a project-local
`global.json` that selects an SDK with `net11.0` support.

## Quick Start

Build the compiler and generated sources:

```bash
scripts/codex-build.sh
```

Compile and run a Raven sample:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rav -o /tmp/raven-sample.dll
dotnet /tmp/raven-sample.dll
```

Inspect compiler views for the same file:

```bash
dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev syntax samples/cases/quote-summary-linq-result-option.rav

dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev bound-tree samples/cases/quote-summary-linq-result-option.rav
```

For a guided walkthrough, including a first `hello.rav` file and project
scaffolding, see [Getting Started](docs/getting-started.md).

## Using `rvn` and `rvnc`

You can run tools explicitly through `dotnet run`:

```bash
dotnet run -f net10.0 --project src/Raven -- dev syntax path/to/file.rav
dotnet run -f net10.0 --project src/Raven.Compiler -- path/to/file.rav -o /tmp/app.dll
```

Or build once and source helper functions for the current shell:

```bash
dotnet build src/Raven/Raven.csproj -f net10.0
dotnet build src/Raven.Compiler/Raven.Compiler.csproj -f net10.0
source scripts/raven-env.sh

rvn dev syntax path/to/file.rav
rvnc path/to/file.rav -o /tmp/app.dll
```

Project commands use the SDK workflow:

```bash
rvn init --type console --name HelloRaven
rvn build HelloRaven.rvnproj
rvn run HelloRaven.rvnproj
```

Equivalent .NET SDK commands work for `.rvnproj` applications:

```bash
dotnet build path/to/App.rvnproj
dotnet run --project path/to/App.rvnproj
```

## Compiler Driver

Direct compiler invocation:

```bash
dotnet run --project src/Raven.Compiler -- <path-to-file> -o <output-file-path>
```

Common options:

- `--framework <tfm>` - target framework.
- `--refs <path>` - additional metadata reference; repeatable.
- `--raven-core <path>` - reference a specific `Raven.Core.dll`.
- `--emit-core-types-only` - embed Raven core shims instead of referencing
  `Raven.Core.dll`.
- `--no-emit` - analyze only.
- `--highlight` - print diagnostics with highlighted source snippets.
- `-o <path>` - output assembly path.
- `-h`, `--help` - show help.

Creating a `.debug/` directory in the current or a parent folder causes the
compiler to emit per-file dumps such as syntax tree, highlighted syntax, raw
source, bound tree, and binder tree into that directory.

`rvn dev` provides console debug views including `syntax`, `dump`, `bound-tree`,
`symbols`, and `quote`.

## Editor Support

The Raven VS Code extension supports F5 compile-and-debug for active `.rav`
files and `.rvnproj` projects. Repository launch presets live in
[.vscode/launch.json](.vscode/launch.json):

- `Raven: Compile and Debug (active file)`
- `Raven: Compile and Debug (project)`

The debug flow compiles through `Raven.Compiler` into `.raven-debug`, then
launches `dotnet <output.dll>` under the debugger. See
[Raven VS Code extension docs](docs/compiler/raven-vscode-extension.md) for
settings such as `raven.sdkPath`, `raven.compilerProjectPath`,
`raven.languageServerPath`, and `raven.targetFramework`.

## Development Notes

- Generated syntax files live under `Syntax/generated/` and
  `Syntax/InternalSyntax/generated/`; do not edit them by hand.
- Generator-affecting changes require `scripts/codex-build.sh`.
- For focused compiler work, use
  [docs/testing/test-impact-map.md](docs/testing/test-impact-map.md) to choose a
  targeted build and test baseline.
- Format touched code files with `dotnet format whitespace ... --include ...`.

## Contributing

Contributions are welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for coding
standards, git conventions, and workflow details.
