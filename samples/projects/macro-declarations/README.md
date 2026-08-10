# Macro declarations

This same-project sample demonstrates Raven's concise macro syntax. The
compiler lowers `macro Double` into the existing local provider contracts,
binds its ordinary `int` parameter, and evaluates the reached `expand`
statement while compiling the invocation.

`AddOffset` shows the other syntax-projection role. Its `offset: int` parameter
binds a typed value, while `expression: ExpressionSyntax` receives the caller's
authored `ExpressionSyntax` rather than requiring a compile-time constant.

`FirstTokenLength` demonstrates the token-tree form without introducing a
separate declaration shape. Its ordinary `offset` parameter is supplied by the
caller, while `tokens: IMacroTokenStream` uses the real Raven.CodeAnalysis
interface and is bound to the
raw `{ ... }` invocation body. The provider class, typed parameter object, and
`TokenTreeMacroContext.CreateTokenStream()` call remain lowering details.

`Declare` demonstrates declaration-list expansion. Its
`SyntaxList<MemberDeclarationSyntax>` return type makes the macro available in
file, namespace, and type-member positions. The file invocation contributes
`Generated`; the invocation inside `Container` contributes the nested type
`Nested`. Both declarations then participate in normal lookup and emission.

The generated provider class and parameter object are implementation details;
the semantic model exposes these declarations as `IMacroDeclarationSymbol`
instances.

Run it with:

```bash
dotnet run --project MacroDeclarations.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
42
6
7
9
```

The other macro projects deliberately retain class-authored implementations as
examples of the underlying provider API and for compatibility coverage.
