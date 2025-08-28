# Raven.Compiler CLI

`Raven.Compiler` is the command-line entry point for the Raven language.
It compiles one or more `.rav` source files into a .NET assembly and exposes a
few switches for inspecting the compiler's output.

## Usage

```bash
dotnet run --project src/Raven.Compiler -- [options] <source-files>
```

## Options

- `-s` &ndash; display the syntax tree (single file only)
- `-d` &ndash; dump syntax with highlighting (single file only)
- `-r` &ndash; print the raw source (single file only)
- `-b` &ndash; print the binder tree (single file only)
- `--ref <path>` &ndash; additional metadata reference
- `--framework <tfm>` &ndash; target framework (e.g. `net8.0`)
- `-o <path>` &ndash; output assembly path

If any of the debug flags above are provided while compiling multiple files or
a `.debug` marker file exists, the output is written into a `debug/` directory
alongside the build. Each input file produces its own syntax tree, highlighted
syntax, raw source and binder tree dump. These files make it easier to debug
the compiler or share reproduction cases.

When no framework is specified the compiler defaults to the newest installed
framework.
