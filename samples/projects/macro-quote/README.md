# Quote Macro (`.rvnproj`)

This sample shows `quote!` in its primary macro-authoring role: constructing the
Raven syntax returned by another macro without manually assembling a syntax
tree.

The Raven-authored `twice!` macro declares an `ExpressionSyntax` parameter, so
the compiler projects the caller's ordinary Raven expression into authored
syntax. Its body expands to:

```raven
quote! {
    #(expression) + #(expression)
}
```

The `#(...)` holes splice the caller's expression syntax into the quoted
expression. `expand` sets that syntax as the final expansion and returns from
the macro. The compiler lowers this concise declaration to the provider
contracts used by the application; the generated class and parameter object
remain implementation details.

Files:

- `app/MacroQuote.rvnproj`: Raven application using `twice!`
- `app/src/Main.rvn`: macro invocation and executable entry point
- `macros/QuoteMacros.rvnproj`: Raven compiler-plugin project
- `macros/TwiceMacro.rvn`: quote-based macro implementation

Run the sample:

```bash
dotnet run --project app/MacroQuote.rvnproj --property WarningLevel=0
```

Expected output:

```text
42
```
