# Macro functions

This same-project sample demonstrates the function-oriented macro syntax. The
compiler lowers `macro func Double` into the existing local provider contracts,
binds its ordinary `int` parameter, and evaluates the reached `expand`
statement while compiling the invocation.

`FirstTokenLength` demonstrates the token-tree form without introducing a
separate declaration shape. Its ordinary `offset` parameter is supplied by the
caller, while `tokens: TokenStream` is a compiler-known input role bound to the
raw `{ ... }` invocation body. The provider class, typed parameter object, and
`TokenTreeMacroContext.CreateTokenStream()` call remain lowering details.

The generated provider class and parameter object are implementation details;
the semantic model exposes both declarations as `IMacroFunctionSymbol`
instances.

Run it with:

```bash
dotnet run --project MacroFunctions.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
6
```

The other macro projects deliberately retain class-authored implementations as
examples of the underlying provider API and for compatibility coverage.
