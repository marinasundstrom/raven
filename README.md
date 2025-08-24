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

### Example

```raven
import System.*
import System.Text.*

let list = [1, 42, 3]
var i = 0

let stringBuilder = new StringBuilder()

while i < list.Length {
    let x = list[i]
    stringBuilder.AppendLine(x.ToString())
    if x > 3 {
        Console.WriteLine("Hello, World!")
    }
    i = i + 1
}

Console.WriteLine(stringBuilder.ToString())
````

**Highlights**:

* Control flow as expressions (`if`, `while`)
* `let` vs `var` (immutable vs mutable)
* Implicit returns (no `return` keyword)
* Array/index access
* Direct interop with .NET libraries

More [samples](src/Raven.Compiler/samples/).

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

# Build and test
dotnet build Raven.sln
dotnet test
```

### Run the compiler

Command:

```bash
dotnet run --project src/Raven.Compiler -- <path-to-file> -o <output-file-path>
```

> ⚠️ **When the arguments are omitted**, there is a hardcoded input file, and and hardcoded output file path (`test.dll`).

---

## 📂 Repository Structure

```
src/
  Raven.CodeAnalysis/         # Compiler core: syntax, binder, semantic model, code gen
  Raven.Compiler/             # Command-line compiler & samples
  Raven.CodeAnalysis.Testing/ # Diagnostic test helpers
  TypeUnionAnalyzer/          # Analyzer for C# type unions
  TestDep/                    # Auxiliary test project

test/                         # Unit tests
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
