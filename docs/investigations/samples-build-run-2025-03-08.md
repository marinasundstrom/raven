# Samples build/run investigation (2025-03-08)

## Scope
Ran the samples build/run procedure from `samples/README.md` after rebuilding the solution to validate the recent constructed-type canonicalization changes and identify new breakages.

## Environment
- DOTNET_ROOT/RAVEN_DOTNET_ROOT set to `~/.dotnet` for `ravc` emission.
- Build configuration: Debug (`dotnet build --property WarningLevel=0`).

## Build procedure
1. Generators:
   - `(cd src/Raven.CodeAnalysis/Syntax && dotnet run --project ../../../tools/NodeGenerator -- -f)`
   - `(cd src/Raven.CodeAnalysis && dotnet run --project ../../tools/BoundNodeGenerator -- -f)`
   - `(cd src/Raven.CodeAnalysis && dotnet run --project ../../tools/DiagnosticsGenerator -- -f)`
2. Build:
   - `dotnet build --property WarningLevel=0`
3. Samples compilation:
   - `RAVEN_CORE=../src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll bash build.sh`
4. Samples execution:
   - `OUTPUT_DIR=output bash run.sh`

## Observations
### Raven.Core path mismatch
`build.sh` reported `Warning: Raven.Core.dll not found; samples will be built without --raven-core` even though `src/Raven.Core/bin/Debug/net9.0/Raven.Core.dll` exists. The fallback path in `build.sh` expects `.../net9.0/net9.0/Raven.Core.dll`, which does not exist.

**Impact:** samples compile without Raven.Core, likely altering failures and diagnostics for union/result samples.

### Compilation failures from build.sh
`build.sh` compiled 64/78 samples and failed 14. Failures include:
- `generic-math-error.rav`: stack overflow recursion in `SymbolEqualityComparer` via `TypeParameterSubstitutionComparer` -> `ConstructedNamedTypeSymbol.Substitute` (seen in output). This appears to be the same recursion path reported earlier and may be exacerbated by the new cache using equality/normalization paths that touch `TypeArguments`.
- `result.rav` and `test-result3.rav`: emission failure with `ArgumentNullException` (`FieldInfo` null) in `ExpressionGenerator.EmitAssignmentExpression`.
- Front-end diagnostics: `async-test.rav`, `extensions.rav`, `introduction.rav`, `linq.rav`, `async/async-await.rav`, `async/async-program-main.rav`, `async/async-task-return.rav`, `async/http-client*.rav`, `async/try-match-async.rav`.

### Runtime failures from run.sh
`run.sh` executed 56/75 output DLLs successfully and failed 19. Failures fall into these categories:
- Missing `*.runtimeconfig.json` (dotnet treats as self-contained): `async-*.dll`, `extensions.dll`, `http-client*.dll`, `introduction.dll`, `linq.dll`, `result.dll`, `test-result3.dll`, `try-match-async.dll`.
- Invalid IL/state machine issues:
  - `async-file-io.dll`: `TypeLoadException` (illegal field type in async state machine)
  - `async-generic-compute.dll`, `async-inference.dll`, `async-try-catch.dll`, `generics.dll`: `BadImageFormatException`
  - `generator.dll`: `TypeLoadException` (`GetEnumerator` not virtual)

## Suspected changes-related issues
- The `generic-math-error.rav` crash still appears in the same substitution path (SymbolEqualityComparer -> TypeParameterSubstitutionComparer -> ConstructedNamedTypeSymbol). The canonicalization cache now uses reference identity, but the recursive path still appears during `TryGetSubstitution` when type-argument normalization triggers equality checks.
- Emission failures in `result.rav` and `test-result3.rav` (null `FieldInfo`) continue; verify whether canonicalization affects symbol binding for synthesized fields.

## Follow-ups
- Fix `build.sh` Raven.Core path (`../src/Raven.Core/bin/Debug/net9.0/Raven.Core.dll`) or pass `RAVEN_CORE` explicitly.
- Investigate recursion in `TypeParameterSubstitutionComparer` for `generic-math-error.rav` after canonicalization changes.
- Investigate codegen null `FieldInfo` in `ExpressionGenerator.EmitAssignmentExpression` for `result.rav`/`test-result3.rav`.
