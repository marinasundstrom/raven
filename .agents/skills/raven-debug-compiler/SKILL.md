---
name: raven-debug-compiler
description: Debug workflow for Raven compiler analysis and emission issues using the Raven.Compiler CLI. Use when investigating parser, binder, lowering, emit, or runtime failures from Raven source samples. Covers repro reduction, CLI flags, analysis-first debugging, and optional IL inspection.
---

# Raven Compiler Debugging

Use this skill when debugging compiler behavior through the CLI.

## Repro Strategy

1. Reduce the issue to a tiny `.rav` sample.
2. Prefer working from the `samples` directory or another minimal repro location.
3. Separate analysis problems from emit or runtime problems before changing code.

## CLI Entry Point

Use `Raven.Compiler`:

```bash
dotnet run --project ../src/Raven.Compiler --property WarningLevel=0 -- <file.rav> -o test.dll
```

## Useful Flags

- `-s` prints the syntax tree
- `-d pretty` prints a colorized syntax tree
- `-bt` prints the bound tree before full lowering
- `--no-emit` stops after analysis
- `--run` runs the program after a successful compilation
- `-ps` shows parsing sequence details

Extra debug switches live in `src/Raven.Compiler/Flags.cs`.
If you change flags, rebuild before running again.

## Preferred Debug Loop

1. Run with `-d pretty -bt --no-emit` first.
2. If the syntax tree is wrong, investigate lexer or parser behavior.
3. If syntax is correct but the bound tree is wrong, investigate binding or semantics.
4. Only enable emit after analysis looks correct.
5. If emit or runtime still fails, inspect the produced assembly or IL.
6. Lock the repro with a focused test in `test/Raven.CodeAnalysis.Tests`.

## IL Inspection

Use `ilspycmd` to inspect `ravc` output when emit behavior is in question:

```bash
dotnet tool install --global ilspycmd
```

Use IL inspection to understand generated output, not as the primary assertion model for stable tests.

## Validation

- Re-run the minimal repro after each change.
- Prefer the smallest possible focused unit test over broad regression coverage for the first lock.
