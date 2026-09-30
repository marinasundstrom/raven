# Target boundary slice ledger

This ledger tracks execution of the [target boundary plan](target-boundaries-and-bootstrap-plan.md).
Investigation and prototypes start on `codex/target-boundaries`, based on
`neoclr` at `b54d2999c`. General changes still require independent main-based
extraction and validation before production integration.

## Slice 0: plan

Committed as `39f42d680`. Establishes sequencing, ownership, and validation gates.
Documentation only; `git diff --check` passed.

## Slice 1: metadata dependency and coverage inventory

The first extraction is context construction and reference resolution, not the
full provider contract. Current dependencies are:

| Path | Owner today | Constraint on extraction |
| --- | --- | --- |
| Select references and metadata core | `Compilation.Setup` | Explicit-only mode must not acquire host fallback references. |
| Normalize paths, deduplicate identities, construct resolver/context | `Compilation.CreateMetadataLoadContext` | First input wins for identical assembly identities; normalized paths are sorted for deterministic fallback selection. |
| Resolve identity or simple name, load bytes | Nested `StreamBackedPathAssemblyResolver` | Preserve exact-identity priority and existing simple-name fallback; do not introduce stricter admission accidentally. |
| Read assembly identities | `Compilation.ReadAssemblyName*` | Browser/WASI use PE metadata instead of host reflection loading. |
| Share paths with runtime resolution | `s_globalAssemblyPathMap` | Resolver registration is consumed by runtime assembly mapping; isolate this dependency explicitly before removing it. |
| Transfer metadata context between snapshots | `AdoptIncrementalReuseFrom`, `TryReuseMetadataLoadContext` | Import/runtime options and portable reference fingerprints gate reuse; no previous compilation may be retained. |
| Load individual references and construct symbols | `LoadMetadataAssembly`, `GetAssembly`, `ReflectionTypeLoader`, `Symbols/PE` | Still reflection-based after the first extraction; later slices must move this ownership. |
| Resolve target types for emit | Runtime assembly maps and `CodeGen` | Keep metadata-only target objects distinct from host-executable types. |

### Existing coverage and gaps

`MetadataImportOptionsTests` covers explicit references, missing core, missing
Console after host compilation, host-to-isolated reuse rejection, and option
copies. `IncrementalCompilationReuseTests` covers source edits, same-path binary
replacement, and discarded compilation/context retention. These form the first
focused baseline.

Add public-behavior checks for the reverse isolated-to-host transition and
isolated-to-isolated reference changes. Resolver precedence and malformed input
handling need dedicated fixtures before changing those policies. Do not treat
the presence of these test classes as complete target-boundary coverage.

The first extraction should be mechanical and preserve policy. Performance
measurement is deferred until an optimization is proposed; no speed or memory
improvement is claimed for moving ownership. Remaining inventory includes
symbol-family consumers, target lowering, macro host contracts, and main branch
differences.

### Next slice

Extract a .NET-specific context factory/resolver from `Compilation`, retaining
the existing context lifetime and explicit registry callback. This factory is
an internal .NET component, not a target-neutral metadata provider. The final
provider must expose semantic contracts rather than `Assembly`/`Type` objects.
Record focused test results before and after extraction in this ledger.

## Slice 2: import transition coverage

Added three cases in `MetadataImportOptionsTests`: isolated-to-host mode changes
and adding/removing Console between explicit-reference snapshots. Assertions
check public symbol lookup, diagnostics against a cold compilation, and that the
prior snapshot keeps its original results. These are characterization tests;
no compiler bug or behavior change is claimed.

Validation on .NET 11 with SDK `11.0.100-rc.1.26425.128`:

- Baseline: `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
  --filter 'FullyQualifiedName~MetadataImportOptionsTests|FullyQualifiedName~IncrementalCompilationReuseTests'
  /property:WarningLevel=0` — 80 passed, no failures/skips.
- After additions: the same test project filtered to `MetadataImportOptionsTests`,
  with `--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`
  — 13 passed, no failures/skips. Referenced projects were built by the baseline.
- Whitespace formatter and `git diff --check` completed. Formatter reported
  workspace-load warnings; no formatting failure occurred.

No neoCLR runtime, .NET Framework, NanoFramework, or browser execution was tested.
