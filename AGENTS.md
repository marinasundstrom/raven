**Scope:** Entire repository.

**Project:** Raven is an experimental .NET compiler inspired by Swift/Rust/F#. Key folders: `src/Raven.CodeAnalysis` (compiler core), `src/Raven.Compiler` (CLI), `test/Raven.CodeAnalysis.Tests` (unit tests), `tools/*Generator` (codegen utilities), and `docs/` (spec & design). Ignore `test/Raven.CodeAnalysis.Samples.Tests`.

**Before making changes:** run the test suite once to establish a baseline.
Use the baseline split to avoid runtime/emission-heavy noise:

```bash
scripts/test-baseline.sh
```

Run runtime/emission-heavy tests separately:

```bash
scripts/test-runtime-isolated.sh
```

**Before building/tests** run the solution build script only when needed:

- The first time you compile in a fresh workspace or after a clean checkout.
- Any time the syntax/bound model inputs that drive generation change (e.g., updates under `tools/*Generator`, `src/Raven.CodeAnalysis` model definitions, or generator config files).

```bash
scripts/codex-build.sh
```

*This will make sure that the essentials (Raven.CodeAnalysis) is being correctly built*

Afterwards, you might just have to run  
`dotnet build <project path> --property WarningLevel=0`  
in order to build specific projects – unless you need to rebuild the entire syntax model or bound nodes.

**Running tests with WarningLevel=0:** use one of the following forms.

```bash
dotnet test <project-file-path> /property:WarningLevel=0
dotnet test /property:WarningLevel=0
```

**Baseline exemption rule:** when establishing baseline for compiler stabilization work, exempt runtime/emission-heavy tests under `Raven.CodeAnalysis.Tests.CodeGen` and `Raven.CodeAnalysis.Tests.Samples` from the baseline pass (covered by `scripts/test-baseline.sh`), then run them in isolation (`scripts/test-runtime-isolated.sh`).

---

### Debugging the Compiler

**Debugging the Raven compiler is done through the CLI project (`Raven.Compiler`).**  
The easiest way to experiment is from the `samples` directory.

#### 1. Compile a Raven file

```bash
dotnet run --project ../src/Raven.Compiler --property WarningLevel=0 -- <file.rav> -o test.dll
```

This compiles `<file.rav>` into `test.dll`.

#### 2. Useful debugging flags

You can combine any of these options:

| Flag | Description |
|----|----|
| `-s` | Print the parsed **syntax tree** |
| `-d pretty` | Print a **colorized** syntax tree |
| `-bt` | Print the **bound tree** (before full lowering) |
| `--no-emit` | Stop after analysis — skip IL generation |
| `--run` | Run the program after successful compilation |
| `-ps` | Show the **parsing sequence** (lexer/parser debugging) |

**Tip:** If you have specified `-bt` and code generation fails, add `--no-emit` to display the bound tree without attempting IL emission.

**Example: inspect syntax + bound tree without emitting IL**

```bash
dotnet run --project ../src/Raven.Compiler --property WarningLevel=0 -- demo.rav -s -bt --no-emit
```

#### 3. Controlling extra diagnostics

More debug switches are defined in:

```
src/Raven.Compiler/Flags.cs
```

If you change any flags, **rebuild the compiler** before running again.

#### 4. Typical investigation flow

1. Start with `-s` → verify the syntax tree matches expectations  
2. Add `-bt` → check semantic binding  
3. Use `-ps` → diagnose parsing problems  
4. Only enable emit when analysis looks correct

#### 5. Debug loop (preferred)

1. Reproduce with a tiny `.rav` file.
2. Run with `-d pretty -bt --no-emit` first (separates binder/semantic issues from IL issues).
3. If analysis is clean, run again with emit/`--run`.
4. Reduce the sample until only the failing construct remains.
5. Lock it with a focused test in `test/Raven.CodeAnalysis.Tests`.

---

**Coding guidelines:** follow idiomatic .NET style; treat compiler components as immutable; prefer diagnostics over exceptions; keep services loosely coupled via interfaces/DI.

**Contribution checklist:**  
- format code with  
  `dotnet format <solution|project> --include <files> --no-restore`  
- run build/test (unless docs-only)  
- keep generated files up to date  
- add/update tests  
- when touching a failing test area, clean it up as you go: update assertions to current syntax/semantics, remove obsolete tests only when behavior is intentionally no longer supported, and replace them with valid coverage for current behavior
- write concise commit messages  
- summarize PRs with relevant diagnostics  
- update specs/grammar/docs alongside feature changes
- update `CHANGELOG.md` for behavior changes (Added/Changed/Removed + impact)
- when adding a new feature, also evaluate Language Service support and update TextMate grammar definitions as needed (e.g., specific constructs, new keywords, contextual words)

**Additional notes:**  
- focus on incremental, additive changes  
- review `docs/` before altering syntax/semantics  
- ask Codex to collapse large diffs  
- be aware that the repo still contains some legacy codegen tests that assert emitted opcodes, lowered IL shape, or other implementation details; treat those as unstable development scaffolding rather than authoritative product coverage
- if temporary development tests of emitted instructions or lowered code are needed while building a feature, keep them under `test/Raven.CodeAnalysis.Tests/CodeGen/Development`
- tests in `test/Raven.CodeAnalysis.Tests/CodeGen/Development` are development-only scaffolding and should be excluded from the normal baseline/runtime stabilization passes
- do not add new tests that verify emitted opcodes or specific lowered instruction sequences for stabilized features; prefer diagnostics, metadata shape, symbol shape, and observable runtime behavior
- when touching legacy emitted-instruction tests, prefer removing them or replacing them with behavior-focused coverage once the feature is established; if they must remain temporarily, keep them in the development-tests folder instead of mixing them with stable suites
- when writing or testing Raven code exposes compiler behavior that is clearly wrong for the intended/spec'd language behavior, fix the compiler instead of just working around it in Raven code
- if the intended behavior is unclear, reduce the repro, check `docs/`, and then fix or clarify the behavior rather than leaving the compiler in a broken/ambiguous state
- compiler bug fixes must be locked with focused tests (syntax/semantic/codegen as appropriate)
- for language server troubleshooting, capture logs to `logs/raven-lsp.log` (create `logs/` if missing) and include the relevant excerpt when reporting hover/completion/definition failures
- inspect `ravc` outputs with `ilspycmd`  
  (install via `dotnet tool install --global ilspycmd`)  
- prefer implementing new features via lowering where possible  
- unit tests can request an `ITestOutputHelper` parameter to write diagnostics via `WriteLine`.

### Language feature checklist (add/update)

When implementing or modifying a language feature, walk this checklist to avoid partial changes:

- **Syntax model:** update syntax definitions/models and regenerate nodes/factories if needed.
- **Tokens/keywords:** add or update token kinds, lexer handling, and keyword classification.
- **Parser:** parse the new construct (including precedence/associativity and recovery paths).
- **Bound tree/model:** add/update bound nodes and any generated bound visitor/rewriter artifacts.
- **Binding/semantics:** bind the construct, enforce rules, and report diagnostics (no crashes).
- **Lowering:** implement in lowering when appropriate; keep semantic and emit behavior aligned.
- **Code generation:** ensure IL/runtime emission paths handle the feature (or intentionally reject with diagnostics).
- **Operations API:** update `OperationKind`, operation interfaces/nodes, `OperationFactory`, and operation tests/docs.
- **Language service/editor:** validate symbol lookup, hover, completion, and diagnostics in LSP/editor paths.
- **Grammar/spec/docs:** update grammar/specification and API docs (`docs/`) for the final behavior.
- **Tests:** add focused syntax, semantic, operations, and codegen tests as applicable.

**External components:**  
No external type-union analyzer project is part of this repository.
