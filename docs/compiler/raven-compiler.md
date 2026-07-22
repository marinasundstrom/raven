# `rvn` and `rvnc`

`rvnc` is the compiler driver used by MSBuild and other build hosts. It keeps to
compiler inputs and outputs: source/project inputs, references, framework
selection, and assembly emission.

`rvn` is the frontend tool. It owns scaffolding and internal development views,
using the shared compiler workspace setup from `Raven.Compiler.Core`.
Application builds should use the .NET SDK surface: `dotnet build` and
`dotnet run --project`.

## Usage

```bash
rvnc [compiler-options] <source-files|project-file.rvnproj>
rvn build [project-file.rvnproj] [dotnet-build-options]
rvn run [project-file.rvnproj] [dotnet-run-options] [-- application-args]
rvn clean [project-file.rvnproj] [dotnet-clean-options]
rvn dev <syntax|dump|macros|binders|bound-tree|symbols|quote> [options] <source-files|project-file.rvnproj>
rvn init [console|classlib] [--name <project-name>] [--framework <tfm>] [--type <console|classlib>] [--force]
rvn --version
rvnc --version
```

For repository development, load local shell helpers after building:

```bash
source scripts/raven-env.sh
```

The helpers define `rvn` and `rvnc` for the current terminal session. Set
`RAVEN_CONFIGURATION` or `RAVEN_FRAMEWORK` before sourcing to use a different
build output.

## Development Environment Setup

During source development there are several supported ways to run the tools:

1. Direct `dotnet run` invocations. This requires no shell aliases and always
   builds the selected project before execution:

   ```bash
   dotnet run -f net10.0 --project src/Raven -- dev syntax path/to/file.rvn
   dotnet run -f net10.0 --project src/Raven.Compiler -- path/to/file.rvn -o /tmp/app.dll
   ```

2. Session helpers. Build the tool projects once, then source the helper script:

   ```bash
   dotnet build src/Raven/Raven.csproj -f net10.0
   dotnet build src/Raven.Compiler/Raven.Compiler.csproj -f net10.0
   source scripts/raven-env.sh
   rvn dev bound-tree path/to/file.rvn
   rvnc path/to/file.rvn -o /tmp/app.dll
   ```

   The script defines shell functions only for the current terminal session. It
   does not edit `.zshrc`, `.bashrc`, or global shell profiles.

3. Application builds. Use the .NET SDK surface for project-based apps:

   ```bash
   dotnet build path/to/App.rvnproj
   dotnet run --project path/to/App.rvnproj
   ```

   `rvn build`, `rvn run`, and `rvn clean` are convenience commands over that
   same SDK workflow:

   ```bash
   rvn build path/to/App.rvnproj
   rvn run path/to/App.rvnproj
   rvn clean path/to/App.rvnproj
   ```

   In this repository, `Directory.Build.props` wires `.rvnproj` projects to the
   local Raven language targets. External source checkouts can set
   `LanguageTargets` and `RavenCompilerHost` explicitly until Raven ships as a
   packaged SDK/build asset.

4. SDK selection. For `net11.0` samples and projects, use a project-local
   `global.json` that selects an SDK with `net11.0` targeting support. The .NET
   CLI chooses the highest installed SDK by default, which may still be too old
   for a future target framework.

The distribution shape should make these repo-relative paths unnecessary:
package `rvn`, `rvnc`, `Raven.LanguageServer`, Raven MSBuild assets, and
`Raven.Core` together so projects can build with ordinary `dotnet build` and
editors can discover the same SDK root.

## `rvnc` Options

- `--framework <tfm>` &ndash; target framework (e.g. `net8.0`)
- `--refs <path>` &ndash; additional metadata reference (repeatable)
- `-o <path>` &ndash; output path (`.rvn`/legacy `.rav` inputs: assembly file path; `.rvnproj` inputs: output directory path)
- `--runtime-async` &ndash; force .NET 11 runtime-async emission for async methods (`Async` method impl flag + `AsyncHelpers.Await` calls when available)
- `--no-runtime-async` &ndash; disable runtime-async emission and keep classic awaiter pattern/state-machine lowering
- `--global-statements` &ndash; enable top-level/global statements (default)
- `--no-global-statements` &ndash; disable top-level/global statements
- `--namespace-members` &ndash; enable namespace-level `func` and `const` declarations (default)
- `--no-namespace-members` &ndash; disable namespace-level `func` and `const` declarations
- `--namespace-member-imports` &ndash; enable namespace lookup/completion promotion from `[TopLevel]` containers (default)
- `--no-namespace-member-imports` &ndash; disable namespace lookup/completion promotion from `[TopLevel]` containers
- `--members-public-by-default` &ndash; class/struct members default to `public`
- `--no-members-public-by-default` &ndash; class/struct members use normal defaults (`private` for class/struct members)
- `--returned-value-handling <default|full|none|info|warning|error>` &ndash; configure the
  built-in returned-value analyzer (`RAV9029`); project files control analyzer mode, while
  `.editorconfig` controls severity
- `--force-returned-value-handling` &ndash; shorthand for treating returned values that are not
  handled as errors
- `--no-emit` &ndash; analyze only; skip assembly emission
- `-h`, `--help` &ndash; show help

## `rvn dev`

`rvn dev` hosts internal debug views outside the compiler binary:

- `rvn dev syntax [flat|group] <input>` &ndash; print syntax tree
- `rvn dev dump [plain|pretty] <input>` &ndash; dump source syntax view
- `rvn dev macros [original|expanded|both] <input>` &ndash; dump macro source views
- `rvn dev binders <input>` &ndash; print binder tree
- `rvn dev bound-tree [original|lowered|both] <input>` &ndash; print binder and bound tree
- `rvn dev symbols [list|hierarchy] <input>` &ndash; inspect symbols
- `rvn dev quote <input>` &ndash; print SyntaxFactory-style tree construction code

## `rvn build`, `rvn run`, and `rvn clean`

These are frontend conveniences over the .NET SDK project workflow:

- `rvn build [project.rvnproj] [dotnet-build-options]` runs `dotnet build`
- `rvn run [project.rvnproj] [dotnet-run-options] [-- application-args]` runs `dotnet run --project`
- `rvn clean [project.rvnproj] [dotnet-clean-options]` runs `dotnet clean`

When the project path is omitted, `rvn` uses the single `.rvnproj` file in the
current directory. The commands do not invoke `rvnc` directly; MSBuild owns
restore, NuGet/package resolution, project references, and language target
selection.

## Init command

Use `init` to scaffold a `.rvnproj` project in the current directory:

```bash
rvn init
```

Generated files:

- `<CurrentDirectoryName>.rvnproj`
- `src/main.rvn`
- `bin/.gitkeep`

Useful init options:

- `--name <project-name>` &ndash; override generated project and assembly name
- `--framework <tfm>` &ndash; set `TargetFramework` in the generated project file
- `console|classlib` &ndash; select the scaffold type (`console` default)
- `--type <console|classlib>` &ndash; compatibility alias for selecting the scaffold type
- `--force` &ndash; overwrite scaffold files if they already exist

When no framework is specified the compiler defaults to the newest installed
framework.

## .NET 11 runtime-async

When the project target framework is `net11.0` (or newer), Raven auto-enables runtime-async emission.

- Async methods are emitted with the async method-impl flag.
- Await sites are emitted as `System.Runtime.CompilerServices.AsyncHelpers.Await(...)` when the compiler host runtime exposes that API.
- Async state-machine synthesis is skipped in this mode.

Important: if you run the compiler driver via `dotnet run`, run it on `net11.0` so `AsyncHelpers` is available:

```bash
dotnet run -f net11.0 --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj
```

If you build or run a `net11.0` `.rvnproj` through MSBuild (`dotnet build` or
`dotnet run --project`), the selected .NET SDK must support `net11.0`. Use a
project-local `global.json` when the machine has multiple SDK bands installed.

Sample project:

- `samples/projects/runtime-async-net11/README.md`

### Classic lowering vs runtime-async

Classic async lowering (runtime-async off):

- Raven synthesizes async state-machine types and rewrites `await` into explicit awaiter calls.
- Generated IL uses `GetAwaiter` / `GetResult` patterns from compiler-generated machinery.
- Async return types in this mode include `Task`, `Task<T>`, `ValueTask`, and `ValueTask<T>`.

What this leaves on the compiler side:

- Async correctness depends on Raven maintaining a full custom state-machine rewriter.
- New await shapes require additional compiler-side lowering/emission work.
- Async lowering bugs surface as compiler-emission complexity (for example around nested async constructs).

What runtime-async fills:

- Raven marks async methods with runtime async metadata and emits `AsyncHelpers.Await(...)` calls.
- `Task` and `Task<int>` entry points are bootstrapped with `AsyncHelpers.HandleAsyncEntryPoint(...)` when targeting a .NET 11 runtime surface that exposes it.
- .NET 11 runtime provides the core async suspension/resume machinery, reducing compiler-generated state-machine complexity.
- Await support for core BCL shapes is now direct (`Task`, `Task<T>`, `ValueTask`, `ValueTask<T>`, and configured awaitables).

Current limitations:

- To emit `AsyncHelpers.Await(...)`, the compiler host process must run on `net11.0` (for example `dotnet run -f net11.0 ...`).
- If the host runtime does not expose `AsyncHelpers`, Raven falls back to awaiter calls (`GetAwaiter`/`GetResult`).
- Raven-specific `Result<..., ...>` entry-point wrappers still use compiler-emitted bridge logic to map success and error payloads to process results.
- Custom task-like return types that rely on `AsyncMethodBuilderAttribute` are not supported yet.
