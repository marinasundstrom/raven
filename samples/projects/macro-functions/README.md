# Macro functions

This same-project sample demonstrates the function-oriented macro syntax. The
compiler lowers `macro func Double` into the existing local provider contracts,
binds its ordinary `int` parameter, and evaluates the reached `expand`
statement while compiling the invocation.

The generated provider class and parameter object are implementation details;
the semantic model exposes `Double` as an `IMacroFunctionSymbol`.

Run it with:

```bash
dotnet run --project MacroFunctions.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
```

The other macro projects deliberately retain class-authored implementations as
examples of the underlying provider API and for compatibility coverage.
