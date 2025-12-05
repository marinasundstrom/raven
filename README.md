# Raven Programming Language

<!---
[![Build](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml/badge.svg)](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml)
[![Tests](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml/badge.svg?event=push)](https://github.com/marinasundstrom/raven/actions/workflows/dotnet.yml)-->

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Docs](https://img.shields.io/badge/docs-available-brightgreen.svg)](docs/)

**Raven** is a modern programming language and experimental compiler inspired by the [.NET Roslyn](https://github.com/dotnet/roslyn) architecture.

✨ **Key traits**:
- **Familiar yet fresh syntax** — looks like Swift, with ideas borrowed from Rust, F#, and C#  
- **Targets the .NET runtime** — compiles directly to IL and integrates with the .NET ecosystem  
- **Immutable, service-oriented design** — a compiler built as an API, following the "Compiler-as-a-Service" philosophy  

Raven is primarily a **learning and exploration project**, aimed at:
- Understanding modern compiler construction
- Experimenting with language design
- Providing a clean API for syntax manipulation and analysis


## 🔰 Language Overview

Raven is a general-purpose, expression-oriented language with an expression-first design that blends functional and imperative paradigms. Its type system supports union and literal types with flow-sensitive typing similar to TypeScript, and it uses `unit` instead of `void`. As a .NET language, Raven interops seamlessly with C#, even shipping a C# analyzer for type unions. The compiler follows the Roslyn "compiler-as-a-service" architecture.

### Example

```raven
import System.Console.*
import System.Collections.Generic.List<int>
import System.Linq.*

alias LedgerEntry = (label: string, value: int)

func shape(values: List<int>) -> List<LedgerEntry> {
    let result = List<LedgerEntry>()
    for each value in values {
        let label = if value < 0 "debit" else "credit"
        result.Add((label: label, value: value))
    }
    return result
}

func summarize(readings: List<int>) -> string {
    let shaped = shape(readings)
    let total = shaped.Aggregate(0, (acc, entry) => acc + entry.value)

    let verdict = if total > 0
        "net positive"
    else if total < 0
        "net negative"
    else
        "balanced"

    return $"processed ${shaped.Count()} items -> ${verdict} (${total})"
}

var ledger = List<int>()
ledger.Add(25)
ledger.Add(-10)
ledger.Add(5)
ledger.Add(0)

WriteLine(summarize(ledger))
```

**Highlights**:

* Expression-first control flow and file-scope functions
* `let` vs `var` (immutable vs mutable) with tuple-shaped data
* Lambdas and LINQ interop against .NET collection types
* String interpolation with tuple element access
* Direct interop with .NET libraries

Read the full [Introduction](docs/introduction.md) for a more detailed overview.

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
# Restore packages
dotnet restore

# Generate syntax nodes (run from the Syntax directory)
cd src/Raven.CodeAnalysis/Syntax
dotnet run --project ../../../tools/NodeGenerator   # add `-- -f` to force regeneration
cd ../../..

# Build the compiler, tests, and Raven.Core (the Option/Result standard library built with ravc)
dotnet build Raven.sln
dotnet test
```

### Run the compiler

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
- `-d [plain|pretty[:no-diagnostics]]` &ndash; dump syntax (`plain` for raw text, `pretty` for highlighted syntax; append `:no-diagnostics` to skip underlines, single file only)
- `--highlight` &ndash; display diagnostics with highlighted source snippets and severity-coloured underlines (covers
  compiler, analyzer, and emit diagnostics)
- `-r` &ndash; print the raw source (single file only)
- `-b` &ndash; print the binder tree (single file only)
- `-bt` &ndash; print the binder and bound tree (single file only)
- `--symbols [list|hierarchy]` &ndash; inspect source symbols (`list` dumps properties, `hierarchy` prints the tree)
- `-h`, `--help` &ndash; show help

`ravc` now ships with and references `Raven.Core.dll` by default. The library is copied next to `ravc` during build, and any compilation also copies it to the output directory of the produced assembly. Use `--raven-core` to point to a different build of Raven.Core, or `--emit-core-types-only` to embed the shimmed core types instead of referencing the DLL.

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

---

## 📂 Repository Structure

```
src/
  Raven.CodeAnalysis/         # Compiler core: syntax, binder, semantic model, code gen
  Raven.Compiler/             # Command-line compiler
  Raven.CodeAnalysis.Testing/ # Diagnostic test helpers
  TypeUnionAnalyzer/          # Analyzer for C# type unions
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
