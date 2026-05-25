---
name: raven-debug-compiler
description: Debug workflow for Raven compiler analysis and emission issues using rvn frontend tooling and the rvnc compiler driver. Use when investigating parser, binder, semantic model, incremental binding, lowering, emit, or runtime failures from Raven source samples. Covers repro reduction, CLI flags, binder-owned state, analysis-first debugging, and optional IL inspection.
---

# Raven Compiler Debugging

Use this skill when debugging compiler behavior through command-line tooling.

## Repro Strategy

1. Reduce the issue to a tiny `.rav` sample.
2. Prefer working from the `samples` directory or another minimal repro location.
3. Separate analysis problems from emit or runtime problems before changing code.

## Tool Entry Points

Use `Raven` / `rvn` for developer workflows such as scaffolding, project
commands, and internal debug views. `rvn` is not the compiler driver:

```bash
dotnet run --project ../src/Raven --property WarningLevel=0 -- dev bound-tree <file.rav>
```

Use `Raven.Compiler` / `rvnc` for compiler-driver behavior. This is the
compiler used by project builds, sample builds, MSBuild integration, and
one-shot compile repros:

```bash
dotnet run --project ../src/Raven.Compiler --property WarningLevel=0 -- <file.rav> -o test.dll
```

## Useful Flags

- `rvn dev syntax` prints the syntax tree
- `rvn dev dump pretty` prints a colorized syntax tree
- `rvn dev bound-tree` prints the bound tree before full lowering
- `--no-emit` stops after analysis
- `rvn dev parse-sequence` shows parsing sequence details

Keep `rvnc` minimal. It is the compiler driver used by MSBuild and should not
grow frontend-only debug commands unless the build integration needs them.
Keep developer-only diagnostics, syntax dumps, and other exploratory commands
on the `rvn dev ...` side.
If you change flags, rebuild before running again.

## Preferred Debug Loop

1. Run with `rvn dev dump pretty <file> --no-emit` and `rvn dev bound-tree <file> --no-emit` first.
2. If the syntax tree is wrong, investigate lexer or parser behavior.
3. If syntax is correct but the bound tree is wrong, investigate binding or semantics.
4. Only enable emit after analysis looks correct, then reproduce compiler
   behavior with `rvnc` or `dotnet run --project ../src/Raven.Compiler -- ...`.
5. If emit or runtime still fails, inspect the produced assembly or IL.
6. Lock the repro with a focused test in `test/Raven.CodeAnalysis.Tests`.
7. Keep documentation current when the investigation changes or clarifies compiler behavior; if expected behavior should be documented but is missing from `docs/`, consider adding it.

## Binder And Semantic Model Checks

- Treat binders as the execution units for binding. First identify the binder that should own the answer: method binder for parameters, block binder for immediate locals/statements/expressions, type/member binders for declarations, etc.
- Treat `Compilation` and `SemanticModel` as the semantic service layer. Public semantic APIs should decide whether to answer from binder-owned state, existing semantic-model caches, available incremental state, or a new lazy bind.
- Prefer fixing the responsible binder or public semantic API over adding a caller-side workaround. Public APIs should decide whether to answer from binder-owned state, cached bound nodes, available incremental state, or a narrow rebind.
- Lazy binding is the expected behavior. A semantic query is allowed to trigger the bind that produces missing information; that information should then live in compiler-owned binder/semantic-model state and be reused by later queries.
- Available-state query helpers are opportunistic fast paths, not the source of truth. If the available path lacks enough context, the authoritative semantic query should bind and cache the correct answer rather than return a guessed symbol or type.
- Do not treat "not already cached" as "not knowable." If a symbol, type, diagnostic, or operation can be resolved by normal binding, every semantic query path that needs it should be able to reach that binding path.
- Cheap available-state paths are useful for performance only when they are sound. If available-state inference is ambiguous or incomplete, fall back to full binding rather than returning a partial answer.
- When debugging incremental behavior, compare one-shot compilation against the semantic query path. The first query after an edit must be correct even if no warm cache exists.
- Binder-owned diagnostics should disappear with invalidated binders. Analyzer diagnostics should stay owned by their analyzer pipeline.
- Binder reuse is an outer snapshot/cache decision, not a binder responsibility. A binder may cache state derived from its syntax/scope, but reuse is only valid when the syntax identity and semantic context still match: parent binder/member signature/import scope/compilation options must remain equivalent.
- If a syntax or semantic-context change invalidates a binder, the replacement binder should recreate its owned symbols and diagnostics lazily. Do not make old binders self-heal or mutate into a new context.
- One-shot compilation remains the correctness baseline. Incremental compilation should reuse valid semantic state only as an optimization over the same observable answers.
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
