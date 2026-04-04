# Raven Programming Language

<!---
[![Build](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml/badge.svg)](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml)
[![Tests](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml/badge.svg?event=push)](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml)-->

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Docs](https://img.shields.io/badge/docs-available-brightgreen.svg)](docs/)

**Raven** is a modern general-purpose programming language and compiler inspired by the [.NET Roslyn](https://github.com/dotnet/roslyn) architecture.

✨ **Key traits**:
- **Expression-first Raven syntax** — explicit `val`/`var`, `match`, `record class`, and `Result`/`Option`-centric flow
- **Targets the .NET runtime** — compiles directly to IL and interoperates with .NET libraries
- **Compiler-as-a-Service architecture** — immutable, service-oriented compiler APIs

Raven is aimed at:
- Building a modern programming language and compiler for .NET
- Providing a clean API for syntax manipulation and analysis
- Documenting language and compiler design clearly


## 🔰 Language Overview

Raven is expression-oriented and blends functional and imperative styles. It uses `()` (`unit`) instead of `void`, models recoverable failure with `Result<T, E>` and absence with `Option<T>`, and uses explicit mutability (`val` vs `var`). As a .NET language, Raven interops directly with existing .NET APIs while keeping Raven-native syntax and semantics.
Type members are public by default; add access modifiers only when you intentionally narrow visibility.

### Example

```raven
import System.*
import System.Console.*
import System.Linq.*
import System.Collections.Generic.*

val plans = List<RatePlan> {
    RatePlan("NorthStar", 500, 120)
    RatePlan("Oceanic", 450, 150)
}

val requests = List<ShipmentRequest> {
    ShipmentRequest("REQ-1001", "NorthStar", 10)
    ShipmentRequest("REQ-1002", "Oceanic", 3)
}

func BuildQuoteSummary(items: IEnumerable<ShipmentRequest>, rates: IEnumerable<RatePlan>) -> Result<QuoteSummary, QuoteError> {
    val request = items.FirstOrError(r => r.Id == "REQ-1002", () => QuoteError("Request not found"))?
    val plan = rates.FirstOrError(r => r.Carrier == request.Carrier, () => QuoteError("Rate plan not found"))?
    val total = plan.BaseCents + (request.WeightKg * plan.PerKgCents)
    return Ok(QuoteSummary(request.Id, request.Carrier, total))
}

val summary = BuildQuoteSummary(requests, plans) match {
    Ok(val item) => "Quote ${item.Id}: ${item.TotalCents} cents"
    Error(val error) => "Quote failed: ${error.Message}"
}

WriteLine(summary)

record class ShipmentRequest(val Id: string, val Carrier: string, val WeightKg: int)
record class RatePlan(val Carrier: string, val BaseCents: int, val PerKgCents: int)
record class QuoteSummary(val Id: string, val Carrier: string, val TotalCents: int)
record class QuoteError(val Message: string)
```

**Highlights**:

* `Result`/`Option` composition with `?` propagation
* `match` as a first-class expression
* `record class` + promoted constructor parameters
* `val`/`var` mutability made explicit
* Direct interop with .NET libraries and LINQ

Read the full [Introduction](docs/introduction.md) and [Getting Started](docs/getting-started.md) for the complete flow.

---

## 🪶 Why the Name "Raven"?

Ravens are remarkable birds, known for their intelligence and adaptability.  

In Old Norse mythology, ravens held significant importance as messengers of Odin. His two ravens, **Huginn** ("thought") and **Muninn** ("memory/mind"), symbolized intellect and reflection—qualities that align with the goals of this language.  

The name reflects both the **mythological roots** and the **clever traits** of these birds.  
Alternative names considered: Old Norse **"Hrafn"** or Danish **"Ravn."**

---

## 🎯 Project Goals

- **Create a Programming Language** — build a language from the ground up, covering design and implementation.  
- **Focus on Parsing & Semantics** — implement parsing, binding, and analysis as the backbone of compilation.  
- **Serve as a Reference** — provide a well-documented example for compiler enthusiasts.  
- **Pragmatic Scope** — aim for a practical subset of Roslyn-like features, not full parity.  

---

## ✨ Syntax

See the pseudo-specification [here](/docs/lang/spec/language-specification.md).

More [samples](samples/).

---
 
## 🧩 API

* Compiler API reference: [docs/compiler/api](docs/compiler/api)
* Example usage: [Raven.Compiler project](src/Raven.Compiler/Program.cs)

---

## 🛠 Prerequisites

* [.NET SDK 9.0](https://dotnet.microsoft.com/)
* Optional: [DocFX](https://dotnet.github.io/docfx/) for docs

---

## 🚀 Quick Start

```bash
# Build essentials
scripts/codex-build.sh

# Run baseline tests (runtime/emission-heavy suites excluded)
scripts/test-baseline.sh
```

Compile and run a sample case:

```bash
dotnet run -f net10.0 --project src/Raven.Compiler --property WarningLevel=0 -- \
  samples/cases/quote-summary-linq-result-option.rvn -o /tmp/raven-sample.dll --run
```

Useful debug flags:

```bash
-s            # syntax tree
-d pretty     # pretty syntax dump
-bt           # binder + bound tree
--no-emit     # analysis only
--run         # execute after successful compile
```

### End-to-end project workflow

Create and run a Raven app project:

```bash
mkdir hello-raven
cd hello-raven

# Create a project scaffold (default type: app)
rvn init

# Build (project files default output to ./bin)
rvn *.rvnproj

# Run the produced assembly
dotnet bin/Hello.dll
```

Create a class library scaffold instead:

```bash
rvn init --type classlib --name MyLibrary
```

Project-system and NuGet details:

- [Compiler project system docs](docs/compiler/project-system.md)
- [NuGet project-file sample](samples/project-files/nuget-demo/README.md)

### Run the compiler manually

Command:

```bash
dotnet run --project src/Raven.Compiler -- <path-to-file> -o <output-file-path>
```

Options:

- `--framework <tfm>` &ndash; target framework
- `--refs <path>` &ndash; additional metadata reference (repeatable)
- `--raven-core <path>` &ndash; reference a specific `Raven.Core.dll`
- `--emit-core-types-only` &ndash; embed Raven core shims instead of using `Raven.Core.dll`
- `-o <path>` &ndash; output assembly path
- `-s` &ndash; display the syntax tree (single file only)
- `-ps` &ndash; prints the tokens as they are being parsed
- `-d [plain|pretty[:no-diagnostics]]` &ndash; dump syntax (`plain` for raw text, `pretty` for highlighted syntax; append `:no-diagnostics` to skip underlines, single file only)
- `--highlight` &ndash; display diagnostics with highlighted source snippets and severity-coloured underlines (covers
  compiler, analyzer, and emit diagnostics)
- `-r` &ndash; print the raw source (single file only)
- `-b` &ndash; print the binder tree (single file only)
- `-bt` &ndash; print the binder and bound tree (single file only)
- `-q`, `--quote` &ndash; print the parsed syntax as compilable C# `SyntaxFactory` code using RavenQuoter defaults (includes trivia, static factory import, and named arguments)
- `--symbols [list|hierarchy]` &ndash; inspect source symbols (`list` dumps properties, `hierarchy` prints the tree)
- `-h`, `--help` &ndash; show help

`rvn` references `Raven.Core.dll` by default. Use `--raven-core` to point to a different build of Raven.Core, or `--emit-core-types-only` to embed shimmed core types instead of referencing the DLL.

Creating a `.debug/` directory in the current or parent folder causes the
compiler to emit per-file dumps (syntax tree, highlighted syntax, raw source,
bound tree, and binder tree) into that directory. The debug options above will additionally
write to the console when compiling a single file.

> ⚠️ **When the arguments are omitted**, there is a hardcoded input file, and a hardcoded output file path (`test.dll`).

### Run the editor

```bash
dotnet run --project src/Raven.Editor -- <path-to-file>
```

When a file path is supplied, the editor opens the file and displays its name in the window title.

### VS Code F5 (compile + debug)

The Raven VS Code extension now supports F5 compile-and-debug for both single files and project files:

- `.rvn` active file
- `.ravenproj` project

Repository launch presets are included in [`.vscode/launch.json`](.vscode/launch.json):

- `Raven: Compile and Debug (active file)`
- `Raven: Compile and Debug (project)`

The debug flow compiles with `Raven.Compiler` into `${workspaceFolder}/.raven-debug`, then launches `dotnet <output.dll>` under the debugger.
For details and configuration options (`raven.sdkPath`, `raven.compilerProjectPath`, `raven.languageServerPath`, `raven.targetFramework`), see [docs/compiler/raven-vscode-extension.md](docs/compiler/raven-vscode-extension.md).

---

## 📂 Repository Structure

```
src/
  Raven.CodeAnalysis/         # Compiler core: syntax, binder, semantic model, code gen
  Raven.Compiler/             # Command-line compiler
  Raven.CodeAnalysis.Testing/ # Diagnostic test helpers
  TestDep/                    # Auxiliary test project

test/                         # Unit tests
samples/                      # Example Raven programs and CLI demos
tools/
  NodeGenerator/              # Generates syntax node code from Model.xml
  Generator/                  # Shared Roslyn generator framework
docs/                         # Language spec & design docs
```

---

## 🔧 Development Notes

* The `RunNodeGenerator` target in `Raven.CodeAnalysis.csproj` runs automatically, but if generated files are missing, run the command manually.
* Generated files reside in `Syntax/generated/` and `Syntax/InternalSyntax/generated/` — **do not edit by hand**.
* Always run `dotnet build` and `dotnet test` before committing.

---

## 🤝 Contributing

Contributions are welcome!
See [CONTRIBUTING.md](CONTRIBUTING.md) for coding standards, git conventions, and workflow.

---

## 📚 Documentation

* Full documentation: [docs/](docs/)
* Unit tests for the language: [Raven.CodeAnalysis.Tests](test/Raven.CodeAnalysis.Tests)

---

💡 *Raven is a playground for exploring compilers and language design — your ideas and contributions can directly shape its evolution!*
