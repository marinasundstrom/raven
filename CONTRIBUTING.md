# Contributing to Raven

Thank you for considering contributing to **Raven**!  
Raven is a learning and exploration project in compiler construction, and contributions of all sizes are welcome: bug reports, documentation improvements, new tests, or even new language features.

---

## 📜 Code of Conduct
Please keep all discussions respectful and constructive.  
This project follows the [Contributor Covenant](https://www.contributor-covenant.org/) code of conduct.

---

## 🛠 Prerequisites

To build Raven locally, you’ll need:

- [.NET 9.0 SDK](https://dotnet.microsoft.com/) or later  
- Git 2.20+  
- (Optional) [DocFX](https://dotnet.github.io/docfx/) if you want to generate docs  
- (Optional) [Graphviz](https://graphviz.org/) for rendering architecture diagrams  

Build and test Raven:

```bash
git clone https://github.com/marinasundstrom/raven.git
cd raven
dotnet build Raven.sln
dotnet test
````

To run the compiler:

```bash
dotnet run --project src/Raven.Compiler
```

---

## 🔄 Workflow

1. **Fork** the repository.
2. **Create a branch** for your work:

   ```bash
   git checkout -b feature/my-change
   ```
3. **Make your changes** and ensure they follow coding standards.
4. **Run tests** locally:

   ```bash
   dotnet test
   ```
5. **Commit with a clear message** (see [Git Conventions](#-git-conventions)).
6. **Push** your branch and open a Pull Request.

---

## 🧑‍💻 Coding Style

Raven follows conventions inspired by Roslyn and .NET:

* Target **C# 12** / **.NET 9.0**.
* Favor **immutability** (`readonly struct`, `ImmutableArray<T>`, etc.).
* Use **PascalCase** for types/methods, **camelCase** for locals/parameters.
* Use `var` only when type is obvious from context.
* Prefer **expression-bodied members** for simple methods.
* Avoid unnecessary allocations — prefer immutable collections over `List<T>` where possible.

Check formatting before committing:

```bash
dotnet format
```

---

## 🔧 Git Conventions

Raven enforces **LF (`\n`) line endings** via `.gitattributes`.

* Do not commit CRLF (`\r\n`) endings.
* If you generated files with wrong endings, run:

  ```bash
  git add --renormalize .
  ```

**Commit message style:**

```
<type>(<scope>): <short summary>
```

**Types:**

* `feat` — new feature
* `fix` — bug fix
* `docs` — documentation changes
* `style` — formatting changes (no code logic)
* `refactor` — code change that isn’t a fix or feature
* `test` — add or modify tests
* `build` — build system or dependency changes

**Examples:**

* `feat(parser): add support for target-typed member access`
* `fix(binder): resolve shadowing diagnostics for local variables`

---

## 🧪 Tests

* All new features should include unit tests.
* Use [xUnit](https://xunit.net/) for testing.
* Tests live in the `test/` directory, mirroring the source layout.

Run the full test suite:

```bash
dotnet test
```

---

## 📂 Project Structure

```
src/
  Raven.CodeAnalysis/         # Compiler core
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

## 🤝 Getting Help

* Open a [GitHub Issue](https://github.com/marinasundstrom/raven/issues) for bugs or feature requests.
* Use Pull Requests for code contributions.
* For architecture details, see [AGENTS.md](docs/AGENTS.md).

---

✨ Thank you for helping make Raven better!