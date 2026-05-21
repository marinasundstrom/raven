---
name: raven-debug-compiler
description: Debug workflow for Raven compiler analysis and emission issues using the Raven.Compiler CLI. Use when investigating parser, binder, semantic model, incremental binding, lowering, emit, or runtime failures from Raven source samples. Covers repro reduction, CLI flags, binder-owned state, analysis-first debugging, and optional IL inspection.
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
7. Keep documentation current when the investigation changes or clarifies compiler behavior; if expected behavior should be documented but is missing from `docs/`, consider adding it.

## Binder And Semantic Model Checks

- Treat binders as the execution units for binding. First identify the binder that should own the answer: method binder for parameters, block binder for immediate locals/statements/expressions, type/member binders for declarations, etc.
- Prefer fixing the responsible binder or public semantic API over adding a caller-side workaround. Public APIs should decide whether to answer from binder-owned state, cached bound nodes, available incremental state, or a narrow rebind.
- Lazy binding is the expected behavior. A semantic query is allowed to trigger the bind that produces missing information; that information should then live in compiler-owned binder/semantic-model state and be reused by later queries.
- Available-state query helpers are opportunistic fast paths, not the source of truth. If the available path lacks enough context, the authoritative semantic query should bind and cache the correct answer rather than return a guessed symbol or type.
- Do not treat "not already cached" as "not knowable." If a symbol, type, diagnostic, or operation can be resolved by normal binding, every semantic query path that needs it should be able to reach that binding path.
- Cheap available-state paths are useful for performance only when they are sound. If available-state inference is ambiguous or incomplete, fall back to full binding rather than returning a partial answer.
- When debugging incremental behavior, compare one-shot compilation against the semantic query path. The first query after an edit must be correct even if no warm cache exists.
- Binder-owned diagnostics should disappear with invalidated binders. Analyzer diagnostics should stay owned by their analyzer pipeline.
- If a language-server bug reproduces as wrong `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, diagnostics, or operations output, fix `Raven.CodeAnalysis` first.

## IL Inspection

Use `ilspycmd` to inspect `ravc` output when emit behavior is in question:

```bash
dotnet tool install --global ilspycmd
```

Use IL inspection to understand generated output, not as the primary assertion model for stable tests.

## Validation

- Re-run the minimal repro after each change.
- Prefer the smallest possible focused unit test over broad regression coverage for the first lock.
