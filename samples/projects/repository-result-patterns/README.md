# Repository Result Patterns

This multi-file sample models repository lookup with a strongly typed ID,
`Result<T, E>`, nominal error unions, and exhaustive pattern matching. It is
also part of the incremental-compilation stabilization corpus because it
exercises cross-file interface implementation, generic substitution, union
payloads, and edits inside nested executable code.

## Build

From the repository root:

```bash
scripts/build-project-samples.sh repository-result-patterns
```

## Run

```bash
dotnet run --project samples/projects/repository-result-patterns/RepositoryResultPatterns.rvnproj --property WarningLevel=0
```
