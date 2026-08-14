# `rvn` and `rvnc`

`rvnc` is the compiler driver used by MSBuild and other build hosts. It keeps to
compiler inputs and outputs: source/project inputs, references, framework
selection, and assembly emission.

`rvn` is the frontend tool. It owns scaffolding and internal development views,
using the shared compiler workspace setup from `Raven.Compiler.Core`.
It also runs a source file as an isolated file-based application. Project
application builds use the .NET SDK surface: `dotnet build` and
`dotnet run --project`.

## Usage

```bash
rvnc [compiler-options] <source-files|project-file.rvnproj>
rvn <file.rvn> [application-args]
rvn build [project-file.rvnproj] [dotnet-build-options]
rvn run <file.rvn> [compiler-options] [-- application-args]
rvn run [project-file.rvnproj] [dotnet-run-options] [-- application-args]
rvn clean [project-file.rvnproj] [dotnet-clean-options]
rvn doctor
rvn dev <syntax|dump|macros|binders|bound-tree|symbols|quote> [options] <source-files|project-file.rvnproj>
rvn init [console|classlib|web|nano] [--name <project-name>] [--framework <tfm>] [--type <template>] [--force]
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
- `--no-framework-references` &ndash; do not add the default .NET targeting-pack
  references for standalone or project compilation
- `--target-core-library <path>` &ndash; add the supplied core library as a
  reference and retarget emitted core type scopes to its assembly identity
- `--refs <path>` &ndash; additional metadata reference (repeatable)
- `--define <symbols>`, `-define <symbols>` &ndash; add conditional-compilation
  symbols; repeat the option or separate symbols with commas or semicolons
- `--constant <name=value>` &ndash; supply or override a typed `extern const`;
  repeat the option for multiple values
- `-o <path>` &ndash; output path (`.rvn`/legacy `.rav` inputs: assembly file path; `.rvnproj` inputs: output directory path)
- `--runtime-async` &ndash; force .NET 11 runtime-async emission for async methods (`Async` method impl flag + `AsyncHelpers.Await` calls when available)
- `--no-runtime-async` &ndash; disable runtime-async emission and keep classic awaiter pattern/state-machine lowering
- `--global-statements` &ndash; enable top-level/global statements (default)
- `--no-global-statements` &ndash; disable top-level/global statements
- `--namespace-members` &ndash; enable namespace-level `func` and `const` declarations (default)
- `--no-namespace-members` &ndash; disable namespace-level `func` and `const` declarations
- `--namespace-member-imports` &ndash; enable namespace lookup/completion promotion from `[TopLevel]` containers (default)
- `--no-namespace-member-imports` &ndash; disable namespace lookup/completion promotion from `[TopLevel]` containers
- `--returned-value-handling <default|full|none|info|warning|error>` &ndash; configure the
  full mode of the built-in unused-result analyzer (`RAV9034`); project files control analyzer mode, while
  `.editorconfig` controls severity
- `--force-returned-value-handling` &ndash; shorthand for treating returned values that are not
  handled as errors
- `--no-emit` &ndash; analyze only; skip assembly emission
- `-h`, `--help` &ndash; show help

Alternative managed runtimes can provide an explicit reference closure instead
of inheriting the compiler host's .NET targeting pack. For example:

```bash
rvnc \
  --no-framework-references \
  --target-core-library path/to/mscorlib.dll \
  --refs path/to/Target.Library.dll \
  --emit-core-types-only \
  app.rvn \
  -o app.dll
```

These switches control Raven's managed assembly emission. Any target-specific
validation, conversion, packaging, or deployment step still runs afterward.
See [Target platforms](target-platforms.md) for the current support levels.

Nullability is enforced from Raven's static type model. Nullable storage is not
implicitly refined by branches; use a typed pattern binding to obtain a
non-null value. See [Nullability and absence](../lang/nullability.md) for the
complete model and the role of `Option<T>`.

## `rvn dev`

`rvn dev` hosts internal debug views outside the compiler binary:

- `rvn dev syntax [flat|group|json] [--syntax-view authored|expanded] <input>`
  &ndash; print the authored or fully macro-expanded syntax tree. JSON mode
  includes the corresponding complete source text and structured
  node/token/trivia data for editor tooling.
- `rvn dev dump [plain|pretty] <input>` &ndash; dump source syntax view
- `rvn dev macros [original|expanded|both] <input>` &ndash; dump macro source views
- `rvn dev binders <input>` &ndash; print binder tree
- `rvn dev bound-tree [original|lowered|both] <input>` &ndash; print binder and bound tree
- `rvn dev symbols [list|hierarchy] <input>` &ndash; inspect symbols
- `rvn dev quote <input>` &ndash; print Raven SyntaxFactory-style tree construction code

`RavenQuoter` emits Raven source by default. API callers can select the legacy
C# rendering with `RavenQuoterOptions.OutputLanguage`.

## File-based applications and project commands

Run a single source file without creating a project:

```bash
rvn run app.rvn
rvn run app.rvn -- first second
rvn app.rvn first second
```

Arguments after `--` belong to the application. Compiler options, such as
`--framework`, occur before the separator. The source uses normal Raven
compilation and execution semantics; generated artifacts live in an isolated
temporary directory and are removed when execution finishes.

An executable file may use the portable `#!/usr/bin/env rvn` shebang and run
directly on Unix-like systems:

```bash
chmod +x app.rvn
./app.rvn first second
```

Project commands remain frontend conveniences over the .NET SDK workflow:

- `rvn build [project.rvnproj] [--constant NAME=VALUE] [dotnet-build-options]`
  runs `dotnet build`
- `rvn run [project.rvnproj] [--constant NAME=VALUE] [dotnet-run-options]
  [-- application-args]` runs
  `dotnet run --project`
- `rvn clean [project.rvnproj] [dotnet-clean-options]` runs `dotnet clean`

When the project path is omitted, `rvn` uses the single `.rvnproj` file in the
current directory. MSBuild owns restore, NuGet/package resolution, project
references, and language target selection for project inputs.

`--constant` is repeatable. When the project also supplies the same external
constant, the command-line value wins.

## Init command

Use `init` to scaffold a `.rvnproj` project in the current directory:

```bash
rvn init
```

Generated files:

- `<CurrentDirectoryName>.rvnproj`
- `src/Main.rvn` (`src/Library.rvn` for class libraries)
- `bin/.gitkeep`

Console projects use an explicit `func Main()` entry point. Class-library
scaffolds contain a declaration instead of executable file-scope code.

Useful init options:

- `--name <project-name>` &ndash; override generated project and assembly name
- `--framework <tfm>` &ndash; set `TargetFramework` in the generated project file
- `console|classlib|web|nano` &ndash; select the scaffold type (`console` default)
- `--type <template>` &ndash; compatibility alias for selecting the scaffold type
- `--list` &ndash; list all available scaffold types
- `--force` &ndash; overwrite scaffold files if they already exist

When no framework is specified the compiler defaults to the newest installed
framework.
The web scaffold defaults to the currently validated ASP.NET target, `net10.0`,
and the nanoFramework scaffold defaults to `netnano1.0` instead.

The canonical scaffold files are also shipped in the `Raven.Templates` NuGet
package for `dotnet new raven-console`, `raven-classlib`, `raven-web`, and
`raven-nano`. Both template channels pin the matching `Raven.Sdk` package in
the generated project so it builds directly through the normal .NET CLI.

## .NET 11 runtime-async

When the project target framework is `net11.0` (or newer), Raven auto-enables runtime-async emission. The target framework's reference assemblies are the authority: Raven only uses runtime async when the target corlib exposes the complete `System.Runtime.CompilerServices.AsyncHelpers` contract, including the runtime entry-point handlers. Earlier frameworks may contain an experimental form of the type without that complete contract.

- Async methods are emitted with the async method-impl flag.
- Await sites are emitted as `System.Runtime.CompilerServices.AsyncHelpers.Await(...)` when the target runtime surface exposes that API.
- Other awaitable patterns use `UnsafeAwaitAwaiter<TAwaiter>` or `AwaitAwaiter<TAwaiter>` after their `IsCompleted` check, matching the runtime suspension protocol.
- Async state-machine synthesis is skipped in this mode.

The distributed compiler host targets .NET 11. If you run the compiler driver from source via `dotnet run`, use its `net11.0` target as well:

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

- The distributed compiler host must run on .NET 11. Source builds should likewise use `dotnet run -f net11.0 ...` when compiling a `net11.0` target.
- A target framework without the complete runtime-async contract, such as `net10.0`, uses classic state-machine lowering even when the compiler itself runs on .NET 11.
- Pass `--no-runtime-async` to opt a direct compiler invocation out explicitly. For an SDK project, set `<RavenUseRuntimeAsync>false</RavenUseRuntimeAsync>`.
- Setting `<RavenUseRuntimeAsync>true</RavenUseRuntimeAsync>` or passing `--runtime-async` explicitly for an unsupported target is rejected instead of producing incompatible output.
- Raven-specific `Result<..., ...>` entry-point wrappers still use compiler-emitted bridge logic to map success and error payloads to process results.
- Custom task-like return types that rely on `AsyncMethodBuilderAttribute` are not supported yet.
