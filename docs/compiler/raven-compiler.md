# Raven.Compiler CLI

`Raven.Compiler` is the command-line entry point for the Raven language.
It compiles one or more `.rav` source files into a .NET assembly and exposes a
few switches for inspecting the compiler's output.

It also supports scaffolding a new Raven project in the current directory via `init`.

## Usage

```bash
dotnet run --project src/Raven.Compiler -- [options] <source-files>
dotnet run --project src/Raven.Compiler -- init [--name <project-name>] [--framework <tfm>] [--type <app|classlib>] [--force]
```

## Options

- `--framework <tfm>` &ndash; target framework (e.g. `net8.0`)
- `--refs <path>` &ndash; additional metadata reference (repeatable)
- `-o <path>` &ndash; output path (`.rav` inputs: assembly file path; `.ravenproj` inputs: output directory path)
- `-s` &ndash; display the syntax tree (single file only)
- `-d [plain|pretty[:no-diagnostics]]` &ndash; dump syntax (`plain` writes the source text, `pretty` emits highlighted syntax; append `:no-diagnostics` to skip diagnostic underlines, single file only)
- `--highlight` &ndash; display diagnostics with highlighted source snippets and severity-coloured underlines (covers
  compiler, analyzer, and emit diagnostics)
- `-r` &ndash; print the raw source (single file only)
- `-b` &ndash; print the binder tree (single file only)
- `-bt` &ndash; print the binder and bound tree (single file only)
- `-h`, `--help` &ndash; show help

## Init command

Use `init` to scaffold a `.ravenproj` project in the current directory:

```bash
dotnet run --project src/Raven.Compiler -- init
```

Generated files:

- `<CurrentDirectoryName>.ravenproj`
- `src/main.rav`
- `bin/.gitkeep`

Useful init options:

- `--name <project-name>` &ndash; override generated project and assembly name
- `--framework <tfm>` &ndash; set `TargetFramework` in the generated project file
- `--type <app|classlib>` &ndash; set `OutputKind` (`app` = `ConsoleApplication`, `classlib` = `DynamicallyLinkedLibrary`; default `app`)
- `--force` &ndash; overwrite scaffold files if they already exist

Creating a `.debug/` directory in the current or parent folder causes the
compiler to emit per-file dumps (syntax tree, highlighted syntax, raw source,
bound tree, and binder tree) into that directory. The debug options above will additionally
write to the console when compiling a single file.

When no framework is specified the compiler defaults to the newest installed
framework.
