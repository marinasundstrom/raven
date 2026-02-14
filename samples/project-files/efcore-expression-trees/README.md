# EF Core Expression Trees (.ravenproj)

This sample is a progress target for Raven expression-tree support with EF Core overlap.

Project file:

- `EfCoreExpressionTrees.ravenproj`
- Package references:
  - `Microsoft.EntityFrameworkCore` `9.0.0`
  - `Microsoft.EntityFrameworkCore.InMemory` `9.0.0`

Source file:

- `src/main.rav`

What it exercises:

- Explicit expression-tree local:
  - `Expression<Func<User, bool>>`
- Common EF-style query chain:
  - `Where` + `OrderBy` + `Select` + `ToList`
- Closure capture inside a predicate (`minAge`)

## Build

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- EfCoreExpressionTrees.ravenproj
```

## Run

```bash
dotnet bin/EfCoreExpressionTrees.dll
```
