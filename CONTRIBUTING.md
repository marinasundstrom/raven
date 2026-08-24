# Contributing to Raven

Thank you for considering contributing to **Raven**! Raven is an experimental
.NET compiler and language project, and contributions of all sizes are welcome:
bug reports, documentation improvements, tests, tooling, and language features.

## Code of Conduct

Please keep all discussions respectful and constructive.
This project follows the [Contributor Covenant](https://www.contributor-covenant.org/) code of conduct.

## Prerequisites

To build Raven locally, you’ll need:

- A .NET 11 SDK. Raven's development tools run on .NET 11 and can target
  .NET 10 when its targeting packs are installed.
- Git.
- Node.js 18+ when building or testing the VS Code extension.
- Bash or Zsh for the repository development-environment scripts.

Clone the repository:

```bash
git clone https://github.com/marinasundstrom/raven.git
cd raven
```

## Build and use the repository toolchain

Build the compiler, Raven Core and macro libraries, command-line tools,
language server, and VS Code extension:

```bash
scripts/build-development-environment.sh
```

This build also creates an ignored SDK view under
`artifacts/development/sdk/Debug/net11.0`. It does not replace an SDK installed
under `~/.raven`.

Choose the workflow that matches what you are testing:

| Workflow | Command | What it uses |
| --- | --- | --- |
| Repository terminal | `scripts/development-shell.sh` | Repository `rvn`, `rvnc`, SDK, Core, and macros |
| Normal VS Code window | `scripts/code-development.sh .` | Installed Raven extension with the repository SDK and language server |
| Isolated extension test | `scripts/code-extension-development.sh .` | Repository extension build with the repository SDK and language server |

The child terminal and VS Code processes receive repository-specific paths
without changing your shell profile, VS Code user settings, installed SDK, or
installed extension. Exit the child shell or close the launched VS Code window
to return to the normal installed environment.

The isolated extension command performs the development build automatically.
Pass `--no-build` when the outputs are already current:

```bash
scripts/code-extension-development.sh --no-build .
```

From a VS Code window already opened on the repository, Run and Debug exposes
three focused configurations:

- **Raven: Test Repository Extension** builds and launches an isolated
  Extension Development Host using all repository artifacts.
- **Raven: Debug Language Server** builds and starts the repository language
  server under the debugger.
- **Raven: Debug Compiler** builds and runs the compiler against the hello
  sample under the debugger.

For an existing Bash or Zsh session, advanced workflows can activate the same
environment directly:

```bash
source scripts/raven-env.sh
raven-env-info

# Restore the environment that existed before activation.
deactivate-raven
```

`RAVEN_CONFIGURATION` and `RAVEN_FRAMEWORK` can select another supported build
before building or activating the environment. The defaults are `Debug` and
`net11.0`; the tool framework supports `net10.0` and `net11.0`.

For more detail, see the [compiler command documentation](docs/compiler/raven-compiler.md)
and [VS Code extension documentation](docs/compiler/raven-vscode-extension.md).

## Run the compiler

Inside the repository development shell, the commands resolve to the build from
this checkout:

```bash
rvnc samples/scripts/hello.rvn -o /tmp/hello.dll
dotnet /tmp/hello.dll
rvn dev syntax samples/scripts/hello.rvn
```

Direct `dotnet run` remains useful when debugging an individual project without
activating the environment:

```bash
dotnet run -f net11.0 --project src/Raven.Compiler -- \
  samples/scripts/hello.rvn -o /tmp/hello.dll
```

## Workflow

1. **Fork** the repository.
2. **Create a branch** for your work:

   ```bash
   git checkout -b feature/my-change
   ```
3. **Make your changes** and ensure they follow coding standards.
4. **Run the smallest relevant tests** locally. Use
   [`docs/testing/test-impact-map.md`](docs/testing/test-impact-map.md) to select
   the appropriate suite or project. Run the full baseline for broad or
   cross-cutting changes:

   ```bash
   scripts/test-baseline.sh
   ```
5. **Commit with a clear message** (see [Git Conventions](#-git-conventions)).
6. **Push** your branch and open a Pull Request.

## Coding Style

Raven follows conventions inspired by Roslyn and .NET:

* Favor **immutability** (`readonly struct`, `ImmutableArray<T>`, etc.).
* Use **PascalCase** for types/methods, **camelCase** for locals/parameters.
* Use `var` only when type is obvious from context.
* Prefer **expression-bodied members** for simple methods.
* Avoid unnecessary allocations — prefer immutable collections over `List<T>` where possible.

### Raven-first infrastructure

Use Raven for new language-facing libraries, tools, build utilities, samples,
and documentation examples when the language and toolchain can express the
requirement reliably. This deliberate dogfooding is part of compiler
stabilization: it exercises the public compiler API, project system,
incremental behavior, diagnostics, and runtime packaging through the same
boundaries users encounter.

C# remains appropriate for the current compiler implementation, bootstrap
host, CLR interop layers, and infrastructure that Raven cannot yet implement
without creating a dependency cycle. Keep those boundaries explicit instead
of introducing a C# dependency merely for convenience. When a missing Raven
capability blocks a Raven-first implementation, document the gap so it can
inform language and compiler priorities.

Compiler API documentation is Raven-first. Use C# examples only when the topic
is specifically C#/.NET integration with Raven, and label that boundary.

Format touched files before committing. Use whitespace formatting by default;
style and analyzer formatting can modify unrelated code:

```bash
dotnet format whitespace <solution-or-project> --include <files> --no-restore
```

## Git Conventions

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

## Tests

* All new features should include unit tests.
* Use [xUnit](https://xunit.net/) for testing.
* Tests live in the `test/` directory, mirroring the source layout.

For feature-scoped work, prefer the feature suite or a focused filter:

```bash
scripts/test-feature-suite.sh <suite>
dotnet test <project-file> --filter '<focused-filter>' /property:WarningLevel=0
```

Use `scripts/test-baseline.sh` for the normal broad baseline and
`scripts/test-runtime-isolated.sh` for runtime/emission-heavy coverage. Do not
run `test/Raven.CodeAnalysis.Samples.Tests` as part of the normal test workflow.

## Project Structure

```
src/
  Raven.CodeAnalysis/         # Compiler core
  Raven.Compiler/             # Command-line compiler & samples
  Raven/                      # rvn developer and project command
  Raven.Core/                 # Raven core library
  Raven.LanguageServer/       # Language server

test/                         # Unit tests
tools/                        # Compiler source generators
docs/                         # Language, compiler, and contributor docs
```

## Getting Help

* Open a [GitHub Issue](https://github.com/marinasundstrom/raven/issues) for bugs or feature requests.
* Use Pull Requests for code contributions.
* For architecture details, start with the [compiler documentation](docs/compiler/index.md).
* Repository-specific build, testing, and engineering rules are in [AGENTS.md](AGENTS.md).
