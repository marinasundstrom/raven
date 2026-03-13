# `rvn` CLI

`rvn` is the command-line entry point for the Raven language.
It compiles one or more `.rvn` source files (with legacy `.rav` compatibility) into a .NET assembly and exposes a
few switches for inspecting the compiler's output.

It also supports scaffolding a new Raven project in the current directory via `init`.

For temporary C# interop, see the project-system page's MSBuild bridge section (`build/Raven.MSBuild.targets`).

## Usage

```bash
rvn [options] <source-files>
rvn init [console|classlib] [--name <project-name>] [--framework <tfm>] [--type <console|classlib>] [--force]
```

## Options

- `--framework <tfm>` &ndash; target framework (e.g. `net8.0`)
- `--refs <path>` &ndash; additional metadata reference (repeatable)
- `-o <path>` &ndash; output path (`.rvn`/legacy `.rav` inputs: assembly file path; `.rvnproj` inputs: output directory path)
- `--publish` &ndash; publish-style output (copies runtime dependencies, emits runtime artifacts, and defaults `.rvnproj` output to `<project-dir>/bin/<Configuration>/publish`)
- `--run` &ndash; execute after successful compile (console apps only); runs from normal output (`bin/<Configuration>` for `.rvnproj`) and stages runtime dependencies there as needed
- `--runtime-async` &ndash; force .NET 11 runtime-async emission for async methods (`Async` method impl flag + `AsyncHelpers.Await` calls when available)
- `--no-runtime-async` &ndash; disable runtime-async emission and keep classic awaiter pattern/state-machine lowering
- `--global-statements` &ndash; enable top-level/global statements (default)
- `--no-global-statements` &ndash; disable top-level/global statements
- `--members-public-by-default` &ndash; class/struct members default to `public`
- `--no-members-public-by-default` &ndash; class/struct members use normal defaults (`private` for class/struct members)
- `-s` &ndash; display the syntax tree (single file only)
- `-d [plain|pretty[:no-diagnostics]]` &ndash; dump syntax (`plain` writes the source text, `pretty` emits highlighted syntax; append `:no-diagnostics` to skip diagnostic underlines, single file only)
- `--highlight` &ndash; display diagnostics with highlighted source snippets and severity-coloured underlines (covers
  compiler, analyzer, and emit diagnostics)
- `-r` &ndash; print the raw source (single file only)
- `-b` &ndash; print the binder tree (single file only)
- `-bt` &ndash; print the binder and bound tree (single file only)
- `-q`, `--quote` &ndash; print parsed syntax as compilable C# `SyntaxFactory` code via RavenQuoter (includes trivia, emits `using` directives, uses static `SyntaxFactory` import, and named arguments)
- `--no-emit` &ndash; analyze only; skip assembly emission
- `-h`, `--help` &ndash; show help

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

Creating a `.debug/` directory in the current or parent folder causes the
compiler to emit per-file dumps (syntax tree, highlighted syntax, raw source,
bound tree, and binder tree) into that directory. The debug options above will additionally
write to the console when compiling a single file.

When no framework is specified the compiler defaults to the newest installed
framework.

## .NET 11 runtime-async

When the project target framework is `net11.0` (or newer), Raven auto-enables runtime-async emission.

- Async methods are emitted with the async method-impl flag.
- Await sites are emitted as `System.Runtime.CompilerServices.AsyncHelpers.Await(...)` when the compiler host runtime exposes that API.
- Async state-machine synthesis is skipped in this mode.

Important: if you run the compiler via `dotnet run`, run the compiler host on `net11.0` so `AsyncHelpers` is available:

```bash
dotnet run -f net11.0 --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj --run
```

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
- .NET 11 runtime provides the core async suspension/resume machinery, reducing compiler-generated state-machine complexity.
- Await support for core BCL shapes is now direct (`Task`, `Task<T>`, `ValueTask`, `ValueTask<T>`, and configured awaitables).

Current limitations:

- To emit `AsyncHelpers.Await(...)`, the compiler host process must run on `net11.0` (for example `dotnet run -f net11.0 ...`).
- If the host runtime does not expose `AsyncHelpers`, Raven falls back to awaiter calls (`GetAwaiter`/`GetResult`).
- Entry-point bridge methods remain synchronous wrappers that block via awaiter calls.
- Custom task-like return types that rely on `AsyncMethodBuilderAttribute` are not supported yet.
