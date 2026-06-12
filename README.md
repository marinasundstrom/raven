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
import System.Text.Json.*
import System.Threading.Tasks.*

async func Main() -> Task {
    val users: User[] = [
        .(1, "Ada", Some("compiler engineer"), .Active),
        .(2, "Bo", None, .Suspended("email bounced"))
    ]

    val label = FindUser(users, 1) match {
        Ok(val user) => user.Display()
        Error(.NotFound(val id)) => "No user with id $id"
    }

    val options = JsonSerializerOptions with {
        WriteIndented = true
        PropertyNamingPolicy = .CamelCase
    }

    await Task.Delay(10)
    WriteLine(JsonSerializer.Serialize(label, options))
}

func FindUser(users: User[], id: int) -> Result<User, LookupError> {
    for user in users {
        if user.Id == id {
            return Ok(user)
        }
    }

    return Error(.NotFound(id))
}

record class User(val Id: int, val Name: string, val Role: Option<string>, val Status: UserStatus) {
    func Display() -> string {
        val role = Role match {
            Some(val value) => value
            None => "member"
        }

        val status = Status match {
            .Active => "active"
            .Suspended(val reason) => "suspended ($reason)"
        }

        return "$Name is $status as a $role"
    }
}

union UserStatus {
    case Active
    case Suspended(reason: string)
}

union LookupError {
    case NotFound(id: int)
}
```

**Highlights**:

* `async`/`await` and direct .NET interop
* Collection expressions and explicit `val`/`var` mutability
* Target-typed member and constructor shorthand such as `.Active` and `.(...)`
* Object initialization with `Type with { ... }`
* `Result`/`Option` for recoverable flow and absence
* `match` as a first-class expression
* `record class` + promoted constructor parameters
* Discriminated unions with typed cases

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
  samples/cases/quote-summary-linq-result-option.rav -o /tmp/raven-sample.dll
dotnet /tmp/raven-sample.dll
```

Useful frontend/debug commands:

```bash
dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev syntax samples/cases/quote-summary-linq-result-option.rav
dotnet run -f net10.0 --project src/Raven --property WarningLevel=0 -- \
  dev bound-tree samples/cases/quote-summary-linq-result-option.rav
```

### Developer environment setup

There are three source-workspace setup modes:

1. Run tools through `dotnet run`. This requires no shell setup and is the most explicit form:

   ```bash
   dotnet run -f net10.0 --project src/Raven -- dev syntax path/to/file.rvn
   dotnet run -f net10.0 --project src/Raven.Compiler -- path/to/file.rvn -o /tmp/app.dll
   ```

2. Build once and source session helpers:

   ```bash
   dotnet build src/Raven/Raven.csproj -f net10.0
   dotnet build src/Raven.Compiler/Raven.Compiler.csproj -f net10.0
   source scripts/raven-env.sh
   rvn dev syntax path/to/file.rvn
   rvnc path/to/file.rvn -o /tmp/app.dll
   ```

   Set `RAVEN_CONFIGURATION` or `RAVEN_FRAMEWORK` before sourcing to select a different build output.

3. Use normal .NET project commands for applications:

   ```bash
   dotnet build path/to/App.rvnproj
   dotnet run --project path/to/App.rvnproj
   ```

   The `rvn` frontend also provides convenience commands over the same SDK
   workflow:

   ```bash
   rvn build path/to/App.rvnproj
   rvn run path/to/App.rvnproj
   rvn clean path/to/App.rvnproj
   ```

   When testing `net11.0` projects, use a project-local `global.json` to pin an SDK that supports `net11.0`; the .NET CLI otherwise selects the highest installed SDK.

Distribution goal: package `rvn`, `rvnc`, the language server, Raven build assets, and `Raven.Core` together so users do not need repo-relative paths. Until then, source checkouts use `Directory.Build.props` for in-repo `.rvnproj` builds, and external projects can set `LanguageTargets`/`RavenCompilerHost` explicitly.

### End-to-end project workflow

Create and run a Raven app project:

```bash
mkdir hello-raven
cd hello-raven

# Create a project scaffold (default type: app)
rvn init

# Build and run through rvn's SDK-backed frontend commands
rvn build
rvn run

```

Create a class library scaffold instead:

```bash
rvn init --type classlib --name MyLibrary
```

Project-system and NuGet details:

- [Compiler project system docs](docs/compiler/project-system.md)
- [NuGet project sample](samples/projects/nuget-demo/README.md)

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
- `--highlight` &ndash; display diagnostics with highlighted source snippets and severity-coloured underlines (covers
  compiler, analyzer, and emit diagnostics)
- `--no-emit` &ndash; analyze only
- `-h`, `--help` &ndash; show help

`rvnc` references `Raven.Core.dll` by default. Use `--raven-core` to point to a different build of Raven.Core, or `--emit-core-types-only` to embed shimmed core types instead of referencing the DLL.

Creating a `.debug/` directory in the current or parent folder causes the
compiler to emit per-file dumps (syntax tree, highlighted syntax, raw source,
bound tree, and binder tree) into that directory.

Use `rvn dev` for console debug views such as `syntax`, `dump`, `bound-tree`,
`symbols`, and `quote`.

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
