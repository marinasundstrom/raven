# Compile Macro (`.rvnproj`)

This sample uses Raven syntax quotation and a dynamic syntax hole to construct
and compile a strongly typed delegate:

```raven
let increment = compile<System.Func<int, int>>! {
    value => #(Raven.CodeAnalysis.Syntax.SyntaxFactory.IdentifierName("value")) + 1
}

WriteLine(increment(41))
```

`compile<TDelegate>!` first creates the Raven `ExpressionSyntax`, using the same
`#(...)` holes as `quote!`, then compiles that syntax at runtime and returns
`TDelegate`. The project does not reference `Raven.CodeAnalysis` explicitly;
the Raven compiler detects the intrinsic and adds the compiler-matched runtime
dependency only for projects that need it.

Run the sample:

```bash
dotnet run --project CompileMacro.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
```
